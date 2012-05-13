-module(engine_op_tests).
-include_lib("eunit/include/eunit.hrl").

set_reg_op_test() ->
    Register = 1,
    Value = 10,
    ?assertMatch({set_register,Register,Value},
		 engine_op:new_operation(set_register,Register,Value)).
    
