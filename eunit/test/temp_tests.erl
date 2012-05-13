-module(temp_tests).
-include_lib("eunit/include/eunit.hrl").

f2c_test() ->
    ?assertEqual(0.0, temp:f2c(32)),
    ?assertEqual(0.0, temp:f2c(32.0)),
    ?assertEqual(5.0, temp:f2c(41)),
    ?assertEqual(5.0, temp:f2c(41.0)).

c2f_test() ->
    ?assertEqual(41.0, temp:c2f(5)),
    ?assertEqual(41.0, temp:c2f(5.0)),
    ?assertEqual(32.0, temp:c2f(0)),
    ?assertEqual(32.0, temp:c2f(0.0)).
    
    
