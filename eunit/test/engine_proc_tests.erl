-module(engine_proc_tests).
-include_lib("eunit/include/eunit.hrl").


start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
	     ?assertEqual(true,engine_proc:exists())
     end
    }.

setup() ->
    engine_proc:new_engine().

cleanup({ok,Pid}) ->
    engine_proc:stop(Pid).

