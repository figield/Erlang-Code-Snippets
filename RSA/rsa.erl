-module(rsa).
%% TODO: finish RSA alg.

-compile(export_all).

%%     long[] code(String Msg) {
%%         long[] Ints = LIB.string_to_int_tab(Msg);
%%         long[] CodedInts = new long[Msg.length()];
%%         for (int i = 0; i < Msg.length();++i){
%%             long I = code1(Ints[i]);
%%             //System.out.println(I);
%%             CodedInts[i] = I;
%%         }        
%%         return CodedInts;
%%     }

%%     String decode(long[] CodedInts) {
%%         long[] DecodedInts = new long[CodedInts.length];
%%         for (int i = 0; i < CodedInts.length;++i){
%%             long I = decode1(CodedInts[i]);
%%             //System.out.println(I);
%%             DecodedInts[i] = I;
%%         }
%%         return LIB.int_tab_to_string(DecodedInts);
%%   

generate_rsa_numbers() ->
    %% select two big prime numbers P and Q
    P = prime:get_prime_number(170),
    Q = prime:get_prime_number(230),    
    M = P * Q,
    F = (P - 1) * (Q - 1),

    %% select not big odd number J, relatively prime to F
    %% (nwd (J, F) = 1)
    J = 157,

    %% find such number T which:
    %% (T*J) MOD F = 1
    %%(so there exist X for which T*J = X*F + 1)
    %% T > 0
    [D=1,_,T] = mymath:ext_Euclid(F, J),    
    
    io:format("nwd(F, J) = ~p~n", [D]),
    io:format("public    = ~p~n", [J]),
    io:format("private   = ~p~n", [T]),
    io:format("P         = ~p~n", [P]),
    io:format("Q         = ~p~n", [Q]),
    io:format("module M  = ~p~n", [M]),
    io:format("module F  = ~p~n", [F]),
   
    {T,J,M}.


code1(I, J, M) ->
    %% X = (I^J) MOD M 
    mymath:powmod(I, J, M).
    
decode1(X, T, M) ->
    %% I = (X^T) MOD M 
    mymath:powmod(X, T, M).
    
main()->
    {T,J,M} = generate_rsa_numbers(),

    Msg = [64], % = "My secret message :)"
    CodedMsg = code1(Msg, J, M),
    DecodedMsg = decode1(CodedMsg, T, M),
    
    io:format("Message: ~p~n", [Msg]),
    io:format("Coded numbers: ~p~n", [CodedMsg]),
    io:format("Deoded message: ~p~n", [DecodedMsg]).

