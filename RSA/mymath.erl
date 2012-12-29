-module(mymath).
-compile(export_all).
-author('Dawid Figiel').

%%======================================================================
%% X mod Y
%%======================================================================
mod(X, Y) -> 
    (X rem Y + Y) rem Y.


%%======================================================================
%% A^B MOD M 
%%======================================================================
powmod(A, B, M) ->
    powmod1(A, B, M, 1).
powmod1(_A, 0, _M, Y) -> 
    trunc(Y);
powmod1(A, B, M, Y) ->
    powmod1(mod(A*A, M), trunc(B/2), M, 
            case mod(B, 2) /= 0 of
                false -> Y;
                true  -> mod(Y * A, M)
            end).

%%======================================================================
%% Returns {D, X, Y} 
%%  where d = nwd(a, b) = ax + by 
%% TODO: have tail recursive and not tail recursive version
%%======================================================================
ext_Euclid(A, 0) -> 
    {D, X1, Y1} = {A, 1, 0},
    %io:format("D = AX + BY~n"),
    %io:format("~p = ~p * ~p + ~p * ~p ~n",[A * X1, A, X1, 0, Y1]),
    {D, X1, Y1};
ext_Euclid(A, B) -> 
    {D, X, Y} = ext_Euclid(B, mod(A, B)),
    {D, X1, Y1} = {D, Y, X - trunc(A/B) * Y},
    %io:format("~p = ~p * ~p + ~p * ~p ~n",[A * X1 + B * Y1, A, X1, B, Y1]),
    {D, X1, Y1}.


code1(I, J, M) ->
    %% X = (I^J) MOD M 
    mymath:powmod(I, J, M).
    
decode1(X, T, M) ->
    %% I = (X^T) MOD M 
    mymath:powmod(X, T, M).


calc(Fun, A, B, C) when is_function(Fun) -> %% safer method
    lists:foldr(fun(A1, Acc)->
                        [Fun(A1,B,C)|Acc]
                end,[],A);
calc(Fun, A, B, C) when is_atom(Fun) ->
    lists:foldr(fun(A1, Acc)->
                        [?MODULE:Fun(A1,B,C)|Acc]
                        %[apply(?MODULE,Fun,[A1,B,C])|Acc]
                end,[],A).
