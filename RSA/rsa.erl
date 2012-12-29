-module(rsa).
-compile(export_all).
-author('Dawid Figiel').

generate_rsa_numbers(P1, Q1) ->
    %% select two big prime numbers P and Q
    P = prime:get_prime_number(P1),
    Q = prime:get_prime_number(Q1),    
    M = P * Q,
    F = (P - 1) * (Q - 1),

    %% select not big odd number J, relatively prime to F
    %% (nwd (J, F) = 1)
    J = 157,

    %% find such number T which:
    %% (T*J) MOD F = 1
    %%(so there exist X for which T*J = X*F + 1)
    %% T > 0
    {D=1,_,T} = mymath:ext_Euclid(F, J),    
    
    io:format("nwd(F, J) = ~p~n", [D]),
    io:format("public    = ~p~n", [J]),
    io:format("private   = ~p~n", [T]),
    io:format("P         = ~p~n", [P]),
    io:format("Q         = ~p~n", [Q]),
    io:format("module M  = ~p~n", [M]),
    io:format("module F  = ~p~n", [F]),
   
    {T,J,M}.

%% X = (I^J) MOD M 
code(I, J, M) ->
    mymath:calc(fun mymath:code1/3, I, J, M).
        
%% I = (X^T) MOD M 
decode(X, T, M) ->
    mymath:calc(decode1, X, T, M).

%% TODO: Handle some exceptions
main(P, Q)->
    {T,J,M} = generate_rsa_numbers(P, Q),

    Msg = "My secret message :)",
    CodedMsg = code(Msg, J, M),
    DecodedMsg = decode(CodedMsg, T, M),
    
    io:format("Message: ~p~n", [Msg]),
    io:format("Coded numbers: ~p~n", [CodedMsg]),
    io:format("Decoded message: ~p~n", [DecodedMsg]).

test()->
    main(170, 230).
