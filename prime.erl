-module(prime).


%%======================================================================
%% Check if P is a prime number
%%======================================================================

%% Version 1:
is_prime(P) when P =< 1 ->
    false;
is_prime(2) ->
    true;
is_prime(P) when (P rem 2) == 0 ->
    false;
is_prime(P) ->
    is_prime(P, 3, trunc(math:sqrt(P)), true).
is_prime(_P, _A, _SqrtP, false) ->
    false;
is_prime(_P, A, SqrtP, true) when A > SqrtP ->
    true;
is_prime(P, A, SqrtP, true) when A =< SqrtP ->
    is_prime(P, A + 2, SqrtP, P rem A /= 0).

%% Version 2:
is_prime2(P) when P =< 1 ->
    false;
is_prime2(P) ->
    is_prime2(P, 2, trunc(math:sqrt(P)), true).
is_prime2(_P, _A, _SqrtP, false) ->
    false;
is_prime2(_P, A, SqrtP, true) when A > SqrtP ->
    true;
is_prime2(P, A, SqrtP, true) when A =< SqrtP ->
    is_prime(P, A + 1, SqrtP, P rem A /= 0).

%%======================================================================
%% Return the biggest prime number from the range 2..N
%%======================================================================
get_prime_number(N) ->
    get_prime_number(2, N).

%% Return the biggest prime number from the range <From, To>
get_prime_number(From, To) when (From rem 2) == 0 ->
    get_prime_number(From + 1, To, 0);
get_prime_number(From, To) ->
    get_prime_number(From, To, 0).

get_prime_number(Ni, Nend, PrimeNumber) when Ni =< Nend ->
    case is_prime(Ni) of
        true -> 
            get_prime_number(Ni + 2, Nend, Ni);
        false -> 
            get_prime_number(Ni + 2, Nend, PrimeNumber)
    end;
get_prime_number(_,_,0) ->
    no_prime_number;
get_prime_number(_,_,PrimeNumber) ->
    PrimeNumber.


%%======================================================================
%% Rabin–Miller primality test
%%======================================================================
%% TODO: implement is_prime
%% TODO: implement searching, base on that method


%%======================================================================
%% Return the biggest prime number from the range 2..N
%%======================================================================

%% Version 1:
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
    case is_prime(Ni) of
        true -> filter_prime_numbers2(Ni+1,Nend,[Ni | PrimeNumbers]);
        false -> filter_prime_numbers2(Ni+1,Nend,PrimeNumbers)
    end;
filter_prime_numbers2(_,_,PrimeNumbers) ->
    PrimeNumbers.

%% Version 3:
filter_prime_numbers3(2) ->
    [2];
filter_prime_numbers3(N) ->
    filter_prime_numbers3(3, N, [], true).
filter_prime_numbers3(Ni,Nend,PrimeNumbers,true) when Ni =< Nend ->
    filter_prime_numbers3(Ni+1,Nend,[Ni-1|PrimeNumbers],is_prime(Ni));
filter_prime_numbers3(Ni,Nend,PrimeNumbers,false) when Ni =< Nend -> 
    filter_prime_numbers3(Ni+1, Nend, PrimeNumbers,is_prime(Ni));
filter_prime_numbers3(Ni,Nend,PrimeNumbers,false) when Ni > Nend ->
    PrimeNumbers;
filter_prime_numbers3(Ni, Nend, PrimeNumbers, true) when Ni > Nend ->
    [Ni - 1 | PrimeNumbers].

%% Version 4:
filter_prime_numbers4(N) when N < 2 ->
    [];
filter_prime_numbers4(2) ->
    [2];
filter_prime_numbers4(3) ->
    [2,3];
filter_prime_numbers4(N) when N rem 2 == 0 ->
    filter_prime_numbers4(5, N-1, [2], true);
filter_prime_numbers4(N) ->
    filter_prime_numbers4(5, N, [2], true).

filter_prime_numbers4(Ni,Nend,PrimeNumbers,true) when Ni =< Nend ->
    filter_prime_numbers4(Ni+2,Nend,[Ni-2|PrimeNumbers],is_prime(Ni));
filter_prime_numbers4(Ni,Nend,PrimeNumbers,false) when Ni =< Nend -> 
    filter_prime_numbers4(Ni+2, Nend, PrimeNumbers,is_prime(Ni));
filter_prime_numbers4(Ni,Nend,PrimeNumbers,false) when Ni > Nend ->
    PrimeNumbers;
filter_prime_numbers4(Ni, Nend, PrimeNumbers, true) when Ni > Nend ->
    [Ni - 2 | PrimeNumbers].


%%======================================================================
%% Some tests and benchmarks
%%======================================================================

%% Check which version of is_prime is the fastest one
test1() ->
    io:format("~nCheck which version of is_prime is the fastest one:~n~n"),
    run1(is_prime,  1000000000000000003), 
    run1(is_prime2, 1000000000000000003).

run1(F, N)->
    io:format("Run method ~p for N = ~p ~n",[F, N]),
    Start = now(),  
    R = ?MODULE:F(N),
    T = timer:now_diff(now(), Start),    
    io:format("Result: ~p, Time: ~p microsec.~n~n",[R, T]).

%% Check which method is the fastest one
%% Also check if each method is giving the same result
test2() ->
    io:format("~nCheck which method is the fastest one:~n~n"),
    R = lists:sort(run2(filter_prime_numbers,  10000000)), 
    R = lists:sort(run2(filter_prime_numbers2, 10000000)),
    R = lists:sort(run2(filter_prime_numbers3, 10000000)),
    R = lists:sort(run2(filter_prime_numbers4, 10000000)).
    
run2(F, N)->
    io:format("Run method ~p for N = ~p ~n",[F, N]),
    Start = now(),  
    R = ?MODULE:F(N),
    T = timer:now_diff(now(), Start),    
    io:format("Time: ~p sec.~n~n",[(T div 1000000)]),
    R.
      
%% Looking for prime number
test3()->
    run3(get_prime_number, 
        900001,   %% from
        1000001).  %% to

run3(F, From, To)->
    io:format("The biggest prime number in range from ~p to ~p~n",
              [From, To]),
    Start = now(),  
    R = ?MODULE:F(From, To),
    T = timer:now_diff(now(), Start),    
    io:format("Time: ~p microsec.~n",[T]),
    R.

tests() ->
    test1(),
    test2(),
    test3().
