-module(alpha).

-compile(export_all).

function1()->
    A = "test1",
    B = fun() ->
                io:format("~p~n",[A])
        end,
    B(),
    function2(B).

function2(B) ->
    B().

test() ->    
    function1(),
    io:format("Anonymous functions have access to variables from the context.~n").

