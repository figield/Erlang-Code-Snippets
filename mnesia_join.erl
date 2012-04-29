-module(mnesia_join).                            
-export([start/0]).                       
-import(lists, [seq/2, map/2]).           
-import(io, [fwrite/1, fwrite/2]).        
-include_lib("stdlib/include/qlc.hrl").   

-record(r1, {a, b}).
-record(r2, {c, d}).

% Start up mnesia and set up two tables, one for each record type
start() ->
    mnesia:start(),
    case mnesia:create_table(r1,[{attributes, record_info(fields, r1)}]) of
        {atomic, ok} -> 
            %% put data to table only once
            map(fun(N) -> 
                        ok = mnesia:dirty_write(r1, #r1{a=N, b=1}) 
                end, lists:seq(1, 100)),            
            ok;
        {aborted,{already_exists,_}} -> 
            ok
    end,
    
    case mnesia:create_table(r2,[{attributes, record_info(fields, r2)}]) of
        {atomic, ok} -> 
            map(fun(N) -> 
                        ok = mnesia:dirty_write(r2, #r2{c=N, d=1}) 
                end, lists:seq(1, 1000000)),            
            ok;
        {aborted,{already_exists,_}} -> 
            ok
    end,

    Query = fun(N, Q) ->
                    T1 = now(),
                    R =  mnesia:transaction(fun() -> qlc:e(Q) end),
                    T2 = now(),
                    case R of
                        {atomic, L} -> 
                            %%fwrite("~p~n", [L]),
                            fwrite("~s ~nResult = ~p entries~n", [N, length(L)]),
                            fwrite("Time of this transaction:~p milisec~n", [timer:now_diff(T2,T1)]);
                        Error -> 
                            fwrite("~s error: ~p~n", [N, Error])
                    end
            end,

    Query("Join by field r1, r2.", 
          qlc:q([{R1#r1.a, R2#r2.c}||
                    R1 <- mnesia:table(r1), 
                    R2 <- mnesia:table(r2),
                    R1#r1.a < R2#r2.c,
                    R2#r2.c =< 100,
                    R1#r1.a >= 4
                ])),

    Query("Join by field r1, r2. Different order of the conditions.", 
          qlc:q([{R1#r1.a, R2#r2.c}||
                    R1 <- mnesia:table(r1), 
                    R2 <- mnesia:table(r2),
                    R2#r2.c =< 100,
                    R1#r1.a >= 4,
                    R1#r1.a < R2#r2.c
                ])),

    %% This one seems to be the fastest one. Less than 1 sec!
    Query("Join by field r2, r1. Different order of the conditions.", 
          qlc:q([{R1#r1.a, R2#r2.c}||
                    R2 <- mnesia:table(r2), 
                    R1 <- mnesia:table(r1),
                    R2#r2.c =< 100,
                    R1#r1.a >= 4,
                    R1#r1.a < R2#r2.c
                ])),
     
    ok.
