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
%% X mod Y
%%======================================================================
mod(X, Y) -> 
    (X rem Y + Y) rem Y.

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

%%======================================================================
%% Return the biggest prime number from the range 2..N
%%======================================================================
get_prime_number(N) ->
    get_prime_number(2, N, 2).

get_prime_number(From, To) ->
    get_prime_number(From, To, 2).

get_prime_number(Ni, Nend, PrimeNumber) when Ni =< Nend ->
    case is_prime(Ni, 2, trunc(math:sqrt(Ni)), true) of
        true -> 
            io:format("~p, ",[Ni]),
            get_prime_number(Ni + 1, Nend, Ni);
        false -> 
            get_prime_number(Ni + 1, Nend, PrimeNumber)
    end;
get_prime_number(_,_,PrimeNumber) ->
    PrimeNumber.

%%======================================================================
%% Return the biggest prime number from the range 2..N
%%======================================================================

%% Version 1:
%% eheap_alloc: Cannot allocate 1781763260 bytes of memory 
%% (of type "old_heap").
%% rsa:get_prime_number(9999999999).
%% Crash dump was written to: erl_crash.dump
filter_prime_numbers(N) ->    
    filter_prime_numbers(2, trunc(math:sqrt(N)), lists:seq(2, N),[]).

filter_prime_numbers(I, L, [P | Numbers], PrimeNumbers) when I =< L ->
    filter_prime_numbers(I+1, L, [ X ||  X <- Numbers, X rem I /= 0], [P|PrimeNumbers]);
filter_prime_numbers(_,_,RestOfPrime,PrimeNumbers) ->
    lists:reverse(PrimeNumbers) ++ RestOfPrime.

%% Version 2:
filter_prime_numbers2(N) ->    
         filter_prime_numbers2(2, N, []).

filter_prime_numbers2(Ni, Nend, PrimeNumbers) when Ni =< Nend ->
    case is_prime(Ni, 2, trunc(math:sqrt(Ni)), true) of
        true -> filter_prime_numbers2(Ni + 1, Nend, [Ni | PrimeNumbers]);
        false -> filter_prime_numbers2(Ni + 1, Nend, PrimeNumbers)
    end;
filter_prime_numbers2(_,_,PrimeNumbers) ->
    PrimeNumbers.

%% Version 3:
filter_prime_numbers3(2) ->
    [2];
filter_prime_numbers3(N) ->
    filter_prime_numbers3(3, N, [], true).

filter_prime_numbers3(Ni, Nend, PrimeNumbers, true) when Ni =< Nend ->    
    filter_prime_numbers3(Ni + 1, Nend, [Ni - 1 | PrimeNumbers], 
                          is_prime(Ni, 2, trunc(math:sqrt(Ni)), true));
filter_prime_numbers3(Ni, Nend, PrimeNumbers, false) when Ni =< Nend ->    
    filter_prime_numbers3(Ni + 1, Nend, PrimeNumbers, 
                          is_prime(Ni, 2, trunc(math:sqrt(Ni)), true));
filter_prime_numbers3(Ni, Nend, PrimeNumbers, false) when Ni > Nend ->
    PrimeNumbers;
filter_prime_numbers3(Ni, Nend, PrimeNumbers, true) when Ni > Nend ->
    [Ni - 1 | PrimeNumbers].


%%======================================================================
%% Check if P is a prime number
%%======================================================================
is_prime(P) ->
    is_prime(P, 2, trunc(math:sqrt(P)), true).

is_prime(_P, _A, _SqrtP, false) ->
    false;
is_prime(_P, A, SqrtP, true) when A > SqrtP ->
    true;
is_prime(P, A, SqrtP, true) when A =< SqrtP ->
    is_prime(P, A + 1, SqrtP, P rem A /= 0).

%%======================================================================
%% Some tests and benchmarks
%%======================================================================

%% Check which method is the fastest one
test1() ->
    io:format("~nCheck which method is the fastest one:~n~n"),
    run(filter_prime_numbers,  10000000), % 213 sec
    run(filter_prime_numbers2, 10000000), % 225 sec
    run(filter_prime_numbers3, 10000000). % 225 sec

run(F, N)->
    io:format("Run method ~p for N = ~p ~n",[F, N]),
    Start = now(),  
    ?MODULE:F(N),
    T = timer:now_diff(now(), Start),    
    io:format("Time: ~p sec.~n~n",[(T div 1000000)]).
    
%% Check if each method is giving the same result
test2() ->
    R = lists:sort(filter_prime_numbers(9998)),
    R = lists:sort(filter_prime_numbers2(9999)),
    R = lists:sort(filter_prime_numbers3(9999)),
    ok.
    
%% Try to find really big prime number
test3()->
    run(get_prime_number, 
        999999999999999999999999999999999999999999999901,   %% from
        999999999999999999999999999999999999999999999999).  %% to

run(F, From, To)->
    io:format("Looking for the prime number from ~p to ~p~n",
              [From, To]),
    Start = now(),  
    ?MODULE:F(From, To),
    T = timer:now_diff(now(), Start),    
    io:format("Time: ~p sec.~n~n",[(T div 1000000)]).
