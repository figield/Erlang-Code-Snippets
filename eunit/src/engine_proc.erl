-module(engine_proc).
-export([new_engine/0,
	 stop/1]).
-export([exists/0]).

-export([init/1]).

new_engine() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
    {ok,state}.

exists() ->
    undefined =/= whereis(?MODULE).

stop(_) ->
    ok.
    

