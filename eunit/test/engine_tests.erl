-module(engine_tests).
-include_lib("eunit/include/eunit.hrl").

engine_creation_test() ->
    ?assertMatch({ok,_},engine:new_engine()).


engine_get_register_test() ->
    {ok,Engine} = engine:new_engine(),    
    ?assertEqual(0,engine:get_register(Engine,1)).

engine_set_reg_test() ->
    Register = 1,
    Value = 10,
    Op = engine_op:new_operation(set_register,Register,Value),
    {ok,Engine} = engine:new_engine(),
    {ok,NewEngine} = engine:execute(Engine,Op),
    ?assertMatch(Value,engine:get_register(NewEngine,Register)).
     

