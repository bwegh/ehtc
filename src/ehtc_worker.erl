-module(ehtc_worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

-export([http_get/2]).
-export([http_get/3]).
-export([http_post/3]).
-export([http_post/4]).

-export([set_url/2]).
-export([set_header/2]).
-export([set_body/2]).
-export([trigger_get/1]).
-export([trigger_post/1]).

-export([close/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          partner = none,
          header = [],
          body = <<>>,
          http_data = #{},
          uri = none,
          client = none,
          method = none,

          gun_ready = false,
          gun_pid = none,
          gun_stream = none,
          gun_monitor = none
}).

%% API.

http_get(Url, Pid) ->
    http_get(Url,[], Pid). 

http_get(Url, Header, Pid) ->
   gen_server:call(Pid, {http_get, Url, Header}, 10000).

http_post(Url, Body, Pid) ->
    http_post(Url, Body, [], Pid).

http_post(Url, Header, Body, Pid) ->
   gen_server:call(Pid, {http_post, Url, Header, Body}, 10000).

-spec start_link(pid()) -> {ok, pid()}.
start_link(Pid) ->
	gen_server:start_link(?MODULE, Pid, []).

-spec set_url(Url :: binary(), Pid :: pid() ) -> ok.
set_url(Url, Pid) ->
	gen_server:call(Pid, {set_url, Url}).

-spec set_header(Header :: list(), Pid :: pid() ) -> ok.
set_header(Header, Pid) ->
	gen_server:call(Pid, {set_header, Header}).

-spec set_body(Body :: binary(), Pid :: pid() ) -> ok.
set_body(Body, Pid) ->
	gen_server:call(Pid, {set_body, Body}).

-spec trigger_get(Pid :: pid() ) -> ok.
trigger_get(Pid) ->
	gen_server:cast(Pid, trigger_get).

-spec trigger_post(Pid :: pid() ) -> ok.
trigger_post(Pid) ->
	gen_server:cast(Pid, trigger_post).

-spec close(pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, stop).

%% gen_server.

init(Pid) ->
	{ok, #state{partner = Pid}}.


handle_call({set_url, Url}, _From, State) ->
    {ok, ConPid, MRef, Uri} = open(Url),
    {reply, ok, State#state{uri=Uri, gun_monitor = MRef, gun_pid=ConPid}};
handle_call({set_header, Header}, _From, State) ->
    {reply, ok, State#state{header = Header}};
handle_call({set_body, Body}, _From, State) ->
    {reply, ok, State#state{body = Body}};
handle_call({http_get, Url, Header}, From, State) ->
    {ok, ConPid, MRef, Uri} = open(Url),
    {noreply, State#state{uri=Uri, gun_monitor = MRef, gun_pid=ConPid,
                            header=Header, client = From, method = get}};
handle_call({http_post, Url, Header, Body}, From, State) ->
    {ok, ConPid, MRef, Uri} = open(Url),
    {noreply, State#state{uri=Uri, gun_monitor = MRef, gun_pid=ConPid,
                            header=Header, client = From, body=Body, method = post}};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(trigger_get, #state{uri = Uri, header=Header, gun_pid=ConPid} = State) ->
    Path = binary:bin_to_list(uri:path(Uri)),
    StreamRef = gun:get(ConPid, Path, Header),
    {noreply, State#state{gun_stream = StreamRef}};
handle_cast(trigger_post, #state{uri = Uri, header=Header,
                                         gun_pid=ConPid, body=Body} = State) ->
    Path = binary:bin_to_list(uri:path(Uri)),
    StreamRef = gun:post(ConPid, Path, Header, Body),
    {noreply, State#state{gun_stream = StreamRef}};
handle_cast(stop, State) ->
    stop_gun(State),
    {stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({gun_up,ConPid, _Scheme}, State) ->
    trigger_automated_processing(State),
    {noreply, State#state{ gun_pid = ConPid, gun_ready = true}};
handle_info({gun_response,ConPid, StreamRef, fin, Status, Header},
            #state{gun_pid=ConPid, gun_stream=StreamRef} = State) ->
    State2 = handle_http_done(set_status_and_header(Status, Header, State)),
	{stop, normal, State2};
handle_info({gun_response,ConPid, StreamRef, nofin, Status, Header}, 
            #state{gun_pid=ConPid, gun_stream=StreamRef} = State) ->
    State2 = set_status_and_header(Status, Header, State),
	{noreply, State2};
handle_info({gun_data,ConPid, StreamRef, nofin, Data},  
            #state{gun_pid=ConPid, gun_stream=StreamRef} = State) ->
    State2 = add_http_data(Data, State),
	{noreply, State2};
handle_info({gun_data,ConPid, StreamRef, fin, Data},
            #state{gun_pid=ConPid, gun_stream=StreamRef} = State) ->
    State2 = handle_http_done(add_http_data(Data, State)),
	{stop, normal, State2};
handle_info({'DOWN',MRef, process, ConPid, Reason}, 
             #state{gun_pid = ConPid, gun_monitor = MRef} = State) ->
    State2 = handle_gun_crash(Reason, State), 
	{stop, normal, State2};
handle_info(_Info, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



trigger_automated_processing(#state{client=none}) ->
    ok;
trigger_automated_processing(#state{method=get}) ->
    trigger_get(self()),
    ok;
trigger_automated_processing(#state{method=post}) ->
    trigger_post(self()),
    ok.



handle_http_done(#state{http_data = InHttpData, gun_monitor = MRef, gun_pid =
                        ConPid} = State) ->
    #{header := Header, status := Status, body := InBody} = InHttpData,
    demonitor(MRef),
    gun:shutdown(ConPid),
    {ok, Body} = uncompress_body_if_needed(InBody, Header),
    HttpData = #{header => Header, status => Status, body => Body},
    send_reply_or_return_from_call({ok,HttpData},State),
    State#state{http_data = HttpData}.
  

handle_gun_crash(Reason, State) ->
    send_reply_or_return_from_call({error, {gun_crash, Reason}},State),
    State.

send_reply_or_return_from_call({ok, HttpData},#state{client=none,partner=Partner}) ->
    Partner ! {http_client_result,HttpData}; 
send_reply_or_return_from_call({error, Reason},#state{client=none,partner=Partner}) ->
    Partner ! {http_client_error,Reason}; 
send_reply_or_return_from_call(Data,#state{client=none,partner=Partner}) ->
    Partner ! Data; 
send_reply_or_return_from_call(Result,#state{client=Client}) ->
    gen_server:reply(Client,Result).


set_status_and_header(Status, Header, State) ->
    Map = #{ header => Header, status => Status , body => <<"">>},
    State#state{http_data = Map}.
    
add_http_data(Data, #state{http_data = #{body := OldBody} = HttpData} = State) ->
    Body = << OldBody/binary, Data/binary >>,
    State#state{http_data = maps:put(body, Body, HttpData)}.

open(Url) ->
    Uri = uri:from_string(Url),
    Host= binary:bin_to_list(uri:host(Uri)),
    Port0 = uri:port(Uri),
    Scheme = uri:scheme(Uri),
    Config = scheme_to_map(Scheme),
    Port = ensure_port(Port0,Scheme),
    {ok, ConPid} = gun:open(Host,Port,Config),
    MRef = monitor(process, ConPid),
    {ok, ConPid, MRef, Uri}. 

uncompress_body_if_needed(Body, Header) when is_list(Header) ->
    Encoding = lists:keyfind(<<"content-encoding">>,1,Header),
    uncompress_body_if_needed(Body,Encoding);
uncompress_body_if_needed(Body, false)  ->
    {ok, Body};
uncompress_body_if_needed(Body, {_, <<"gzip">>})  ->
    {ok, zlib:gunzip(Body)};
uncompress_body_if_needed(Body, {_, <<"deflate">>})  ->
    Z  = zlib:open(),
    ok = zlib:inflateInit(Z),
    {ok, zlib:inflate(Z,Body)}; 
uncompress_body_if_needed(_Body, {_, Compression})  ->
    erlang:error({unsupported_encoding, Compression}).
    

stop_gun(#state{gun_pid = Pid}) ->
    gun:close(Pid),
    ok.


scheme_to_map(<<"http">>) ->
    #{};
scheme_to_map(<<"https">>) ->
    #{transport => ssl};
scheme_to_map(_) ->
    #{transport => ssl}.


ensure_port(undefined,<<"http">>) ->
    80;
ensure_port(undefined,<<"https">>) ->
    443;
ensure_port(Port,_) when is_number(Port) ->
    Port;
ensure_port(_Port,_)  ->
    443.


