-module(kill_process).
-vsn(1.1).
-compile(export_all).
%-export([x/0]).


main()->
    register(procB, spawn(?MODULE, proc_b, [])), 
    spawn(?MODULE, proc_d,[]),
    timer:sleep(3000),
    io:format("Process C is alive: ~p~n",[whereis(procC)]),
    io:format("Process A is alive: ~p~n",[whereis(procA)]),
    io:format("Process B is alive: ~p~n",[whereis(procB)]).

proc_d() ->
    io:format("Process D (~p) is created~n",[self()]),
    timer:sleep(1000),
    %%exit(whereis(procB), kill),
    exit(whereis(procB), other),
    receiver("Process D").

proc_b() ->
    io:format("Process B (~p) is created~n",[self()]),
    process_flag(trap_exit, true),
    register(procA, spawn_link(?MODULE, proc_a, [])),
    register(procC, spawn_link(?MODULE, proc_c, [])),
    receiver("Process B").

proc_a() ->
    io:format("Process A (~p) is created~n",[self()]),
    receiver("Process A").

proc_c() ->
    io:format("Process C (~p) is created~n",[self()]),
    process_flag(trap_exit, true),
    receiver("Process C").

receiver(Name) ->    
    receive
        What ->
            io:format("~s has received ~p~n",[Name, What]),
            receiver(Name)
    end.



%% =======================================\

start_tcp_server_process()->
    spawn(fun() ->
                  tcp_server()
          end).

tcp_server()->
    {ok, Listen} = gen_tcp:listen(8096, [{active, false}]),        
    io:format("After listen~n",[]),
    spawn(fun() ->
                  A = gen_tcp:accept(Listen),
                  io:format("After accept:~p~n",[A]),
                  {ok, Accept} = A,
                  R1 = gen_tcp:recv(Accept, 0, 300000),
                  io:format("~p~n",[R1]),
                  %io:format("kill the listening process!~n"),
                  %timer:sleep(30000),
                  R2 = gen_tcp:recv(Accept, 0, 300000),
                  io:format("~p~n",[R2])
                  %receive X -> io:format("X:~p~n",[X]) end,
          end),
    timer:sleep(300000),
    gen_tcp:close(Listen),
    exit(mysterious_reason).

kill_tcp_server_process(Pid)->
    exit(Pid,kill).


start_client()->
    {ok, Socket} = gen_tcp:connect({127,0,0,1},8096,[{packet,0}]),
    Socket.
    
stop_client(Socket)->
    ok = gen_tcp:close(Socket).

send(Socket, Data) ->
    gen_tcp:send(Socket, Data).
