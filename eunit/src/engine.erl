-module(engine).
-export([new_engine/0]).
-export([get_register/2]).
-export([execute/2]).

-record(engine,
	{reg_1 = 0}).

new_engine() ->
    {ok,#engine{}}.

get_register(#engine{reg_1 = Reg},1) ->
    Reg.

execute(Engine, {set_register,1,Value}) ->
    {ok,Engine#engine{reg_1 = Value}}.
