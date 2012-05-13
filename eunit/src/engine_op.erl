-module(engine_op).
-export([new_operation/3]).

new_operation(set_register,Register,Value) ->
    {set_register,Register,Value}.
