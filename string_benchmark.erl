-module(string_benchmark).

%% http://www.erlang.org/doc/efficiency_guide/myths.html#id60508
%% http://www.erlang.org/doc/efficiency_guide/listHandling.html
%% [erlang-questions] Which is best? string:concat or ++?:
%%   https://groups.google.com/forum/?fromgroups#!topic/erlang-programming/0Vm32OMnMDU

%% Fun... fun fun fun
%% and '++' rules, somehow...

-compile(export_all).

-define(len, 1000).
-define(loopN, 1000000).
-define(x(S, Len), string:copies(S, Len)).
-define(x(S), string:copies(S,?len)).

test() ->
    io:format("~nCheck if each method is giving the same result:~n~n"),
    io:format("1. plus_plus:     ~p~n",  [?x("a",4) ++ ?x("b",4) ++ ?x("c",4) ++ ?x("d",4)]),
    io:format("2. lists_flatten: ~p~n",  [lists:flatten([?x("a",4), ?x("b",4),?x("c",4), ?x("d",4)])]),
    io:format("3. lists_concat:  ~p~n",  [lists:concat ([?x("a",4), ?x("b",4),?x("c",4), ?x("d",4)])]),
    io:format("4. lists_append:  ~p~n",  [lists:append ([?x("a",4), ?x("b",4),?x("c",4), ?x("d",4)])]),
    io:format("5. binary_list:   ~p~n",  [binary2list  ([?x("a",4), ?x("b",4),?x("c",4), ?x("d",4)])]),
    io:format("6. concat_binary: ~p~n~n",[binary_to_list(list_to_binary([?x("a",4), ?x("b",4),?x("c",4), ?x("d",4)]))]),
                                         
    A = ?x("a"),
    B = ?x("b"),
    C = ?x("c"),
    D = ?x("d"),

    LoopDiff = run(empty, ?loopN, 0, A, B, C, D),        
    run(plus_plus,     ?loopN, LoopDiff, A, B, C, D), 
    run(lists_flatten, ?loopN, LoopDiff, A, B, C, D),
    run(lists_concat,  ?loopN, LoopDiff, A, B, C, D),
    run(lists_append,  ?loopN, LoopDiff, A, B, C, D),
    run(binary_list,   ?loopN, LoopDiff, A, B, C, D),
    run(concat_binary,   ?loopN, LoopDiff, A, B, C, D).

run(F, N, LoopDiff, A, B, C, D)->
    io:format("Run method ~p ~p times...~n",[F, N]),
    Start = now(),  
    ?MODULE:F(N, A, B, C, D),
    T = timer:now_diff(now(), Start) - LoopDiff,    
    io:format("Time: ~p sec.~n~n",[(T div 1000000)]),
    T.


%% 0
empty(0, _A, _B, _C, _D) -> 
    ok;
empty(N, A, B, C, D)->
    %% Just to see how much time it takes to run the loop only
    empty(N - 1, A, B, C, D).

%% 1
plus_plus(0, _A, _B, _C, _D) -> 
    ok;
plus_plus(N, A, B, C, D)->
    _ = A ++ B ++ C ++ D ++ integer_to_list(N),
    plus_plus(N - 1, A, B, C, D).

%% 2
lists_flatten(0, _A, _B, _C, _D)->
    ok;
lists_flatten(N, A, B, C, D)->
    _ = lists:flatten([A, B, C, D, integer_to_list(N)]),    
    lists_flatten(N - 1, A, B, C, D).

%% 3
lists_concat(0, _A, _B, _C, _D)->
    ok;
lists_concat(N, A, B, C, D)->
    _ = lists:concat([A, B, C, D, integer_to_list(N)]),
    lists_concat(N - 1, A, B, C, D).
    
%% 4
lists_append(0, _A, _B, _C, _D)->
    ok;
lists_append(N, A, B, C, D)->
    _ = lists:append([A, B, C, D, integer_to_list(N)]),
    lists_append(N - 1, A, B, C, D).

%% 5
binary_list(0, _A, _B, _C, _D)->
    ok;
binary_list(N, A, B, C, D) ->
    _ = binary2list([A, B, C, D, integer_to_list(N)], <<>>),
    binary_list(N - 1, A, B, C, D).

binary2list(ListOfList) ->
    binary2list(ListOfList, <<>>).

binary2list([], Acc)->
    binary_to_list(Acc);
binary2list([H|T], Acc)->
    HB = list_to_binary(H),
    binary2list(T, << Acc/binary, HB/binary >>).

%% 6
concat_binary(0, _A, _B, _C, _D)->
    ok;
concat_binary(N, A, B, C, D) ->
    _ = binary_to_list(list_to_binary([A, B, C, D, integer_to_list(N)])),
    concat_binary(N - 1, A, B, C, D).


%% > string_benchmark:test().

%% Check if each method is giving the same result:

%% 1. plus_plus:     "aaaabbbbccccdddd"
%% 2. lists_flatten: "aaaabbbbccccdddd"
%% 3. lists_concat:  "aaaabbbbccccdddd"
%% 4. lists_append:  "aaaabbbbccccdddd"
%% 5. binary_list:   "aaaabbbbccccdddd"
%% 6. concat_binary: "aaaabbbbccccdddd"

%% Run method empty 1000000 times...
%% Time: 0 sec.

%% Run method plus_plus 1000000 times...
%% Time: 29 sec.

%% Run method lists_flatten 1000000 times...
%% Time: 323 sec.

%% Run method lists_concat 1000000 times...
%% Time: 30 sec.

%% Run method lists_append 1000000 times...
%% Time: 29 sec.

%% Run method binary_list 1000000 times...
%% Time: 64 sec.

%% Run method concat_binary 1000000 times...
%% Time: 59 sec.
