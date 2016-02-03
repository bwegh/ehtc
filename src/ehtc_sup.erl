-module(ehtc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_client/0]).
-export([init/1]).

start_client() ->
    supervisor:start_child(?MODULE,[self()]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [worker()],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.

worker() ->
    #{ id => worker,
       start => {ehtc_worker,start_link,[]},
       restart => transient
     }.
