-module(rsa).

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

-define(lib, ?MODULE).
               
generate_rsa_numbers() ->
    %% select two big prime numbers P and Q
    P = ?lib:get_prime_number(170),
    Q = ?lib:get_prime_number(230),    
    M = P * Q,
    F = (P - 1) * (Q - 1),

    %% select not big odd number J, relatively prime to F
    %% (nwd (J, F) = 1)
    J = 157,

    %% find such number T which:
    %% (T*J) MOD F = 1
    %%(so there exist X for which T*J = X*F + 1)
    %% T > 0
    [D=1,_,T] = ?lib:ext_Euclid(F, J),    
    
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
    ?lib:powmod(I, J, M).
    

decode1(X, T, M) ->
    %% I = (X^T) MOD M 
    ?lib:powmod(X, T, M).
    

main()->
    {T,J,M} = generate_rsa_numbers(),

    Msg = [64], % = "My secret message :)"
    CodedMsg = code1(Msg, J, M),
    DecodedMsg = decode1(CodedMsg, T, M),
    
    io:format("Message: ~p~n", [Msg]),
    io:format("Coded numbers: ~p~n", [CodedMsg]),
    io:format("Deoded message: ~p~n", [DecodedMsg]).


%% Return A^B MOD M 
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
 
mod(X, Y) -> 
    (X rem Y + Y) rem Y.

%% Returns d, x, y 
%%  where d = nwd(a, b) = ax + by 
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

%% TODO: have tail recursive and not tail recursive


%%     public static long get_prime_number(long n){
%%         boolean[] numbersTable = new boolean[(int) (n+1)];
%%         long LastPrimeNumber = 2;
%%         //sieve of Eratosthenes
%%         for(int i = 2; i*i <= n; i++){
%%                 if (numbersTable[i] == true)
%%                     continue;
%%                 for (int j = 2 * i ; j <= n; j += i)
%%                     numbersTable[j] = true;                
%%             }
%%         for (int i = (int) n; i >= 2; i--)
%%             if (numbersTable[i] == false){
%%                 System.out.println(i);
%%                 LastPrimeNumber = i;
%%                 break;
%%             }
%%         return LastPrimeNumber;
%%     }
    
%% Return the biggest prime number from the range 2..N
get_prime_number(N) ->
    filter_prime_numbers(2, trunc(math:sqrt(N)), lists:seq(2, N),[]).

filter_prime_numbers(I, L, [P | Numbers], PrimeNumbers) when I =< L ->
    filter_prime_numbers(I+1, L, [ X ||  X <- Numbers, X rem I /= 0], [P|PrimeNumbers]);
filter_prime_numbers(_,_,RestOfPrime,PrimeNumbers) ->
    lists:reverse(PrimeNumbers) ++ RestOfPrime.
    
%% TODO: IMPROVE!
%% rsa:get_prime_number(9999999999).

%% Crash dump was written to: erl_crash.dump
%% eheap_alloc: Cannot allocate 1781763260 bytes of memory (of type "old_heap").
