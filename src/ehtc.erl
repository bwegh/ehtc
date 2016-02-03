-module(ehtc).

-export([http_get/1]).
-export([http_get/2]).
-export([http_post/2]).
-export([http_post/3]).
-export([http_sync_get/1]).
-export([http_sync_get/2]).
-export([http_sync_post/2]).
-export([http_sync_post/3]).
-export([close/1]).


http_sync_get(Url) ->
    http_sync_get(Url, []).

http_sync_get(Url, Header) ->
    {ok, Pid} = ehtc_sup:start_client(),
    ehtc_worker:http_get(Url, Header, Pid). 

http_sync_post(Url, Body) ->
    http_sync_post(Url, [], Body).

http_sync_post(Url, Header, Body) ->
    {ok, Pid} = ehtc_sup:start_client(),
    ehtc_worker:http_post(Url, Header, Body, Pid). 

http_get(Url) ->
    http_get(Url, []).

http_get(Url, Header) ->
    {ok, Pid} = ehtc_sup:start_client(),
    ok = ehtc_worker:set_url(Url, Pid),
    ok = ehtc_worker:set_header(Header, Pid),
    ok = ehtc_worker:trigger_get(Pid),
    {ok, Pid}. 


http_post(Url, Body) ->
    http_post(Url, [], Body).

http_post(Url, Header, Body) ->
    {ok, Pid} = ehtc_sup:start_client(),
    ok = ehtc_worker:set_url(Url, Pid),
    ok = ehtc_worker:set_header(Header, Pid),
    ok = ehtc_worker:set_body(Body, Pid),
    ok = ehtc_worker:trigger_post(Pid),
    {ok, Pid}. 


close(Pid) ->
    ehtc_worker:close(Pid).
