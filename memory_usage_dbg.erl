
%% @doc
%% library functions for debugging.
%%
%% @end

-module(memory_usage_dbg).

%%% ----------------------------------------------------------
%%% EXPORT
%%% ----------------------------------------------------------
-export([get_port_info/0]).
-export([get_process_info/0]).
-export([process_mon/1]).
-export([run_process_mon/2]).
-export([get_memory_info/0]).
-export([memory_mon/1]).
-export([run_memory_mon/4]).

%%%===========================================================================
%% @spec get_port_info() 
%%            ->
%%               [term()] | {error, Reason}
%% where 
%%
%%       Reason = term()
%% @doc 
%% Function to get the list of the ports' information. For debugging purposes.
%% @end
%%%===========================================================================
get_port_info() ->
    [erlang:port_info(X) || X <- erlang:ports()].

%%%===========================================================================
%% @spec get_process_info() 
%%            ->
%%               [term()] | {error, Reason}
%% where 
%%
%%       Reason = term()
%% @doc 
%% Function to get the list of the process information. For debugging purposes.
%% @end
%%%===========================================================================
get_process_info() ->
    PortInfo = get_port_info(),
    R = lists:map(fun(Port) ->
                          case proplists:get_value(name, Port) of 
                              "SOME NAME" ->
                                  PidList = proplists:get_value(links, Port),
                                  lists:map(fun (Pid) -> erlang:process_info(Pid) end, PidList);
                              A ->                           
                                  A
                          end
                  end,
                  PortInfo),
    ct:pal("ProcessInfo: ~p",[R]),
    R.

%%%===========================================================================
%% @spec process_mon(NodeAddress) 
%%            ->
%%               any() | {error, Reason}
%% where 
%%
%%       NodeAddresses = [term()]
%%       Reason = term()
%% @doc 
%% For debugging purposes.
%% Usage: 
%% 1. Run separate Erlang node  during the test case execution:
%%  erl -sname nodename \ 
%%      -setcookie secret_cookie \
%%      -s sp_dbg process_mon testnode@host124
%%  Note: The cookie should be the same as on the node with running test.
%% @end
%%%===========================================================================
process_mon([NodeAddress])->
    run_process_mon(NodeAddress, 30000).

run_process_mon(NodeAddress, SleepTime) ->
    R = rpc:call(NodeAddress, sp_dbg, get_process_info, []),
    io:format("~p~n",[R]),
    timer:sleep(SleepTime),
    run_process_mon(NodeAddress, SleepTime).

%%%===========================================================================
%% @spec get_memory_info() 
%%            ->
%%               [term()] | {error, Reason}
%% where 
%%
%%       Reason = term()
%% @doc 
%% Returns a list containing information about memory dynamically allocated 
%% by the Erlang emulator. For more go to erlang:memory/0.
%% @end
%%%===========================================================================
get_memory_info() ->
    erlang:memory().

%%%===========================================================================
%% @spec memory_mon([NodeAddress]) 
%%            ->
%%               any() | {error, Reason}
%% where 
%%
%%       NodeAddresses = [term()]
%%       Reason = term()
%% @doc 
%% For debugging purposes.
%% Usage: 
%% 1. Run separate Erlang node  during the test case execution:
%%
%%  Ex:
%%
%%  erl -sname nodename \
%%      -setcookie secret_cookie \
%%      -s sp_dbg memory_mon testnode@host124 \ 
%%                           /path/to/rrd_database.rrd
%%
%%  Note: The cookie should be the same as on the node with running test.
%%
%% @end
%%%===========================================================================
memory_mon([NodeAddress, RRD])->
    RRDDB = "rrdtool update " ++ atom_to_list(RRD) ++ " ",
    timer:sleep(5000),
    run_memory_mon(NodeAddress, RRDDB, 30000, 0).

run_memory_mon(NodeAddress, RRDDB, SleepTime, LoopNo) ->
    case rpc:call(NodeAddress, sp_dbg, get_memory_info, []) of
        [{total, Total},
         {processes,Processes},
         {processes_used, Processes_used},
         {system, System},
         {atom, Atom},
         {atom_used, Atom_used},
         {binary, Binary},
         {code, Code},
         {ets, Ets}] ->       
            UpdateValues = lists:flatten(
                             io_lib:format("N:~p:~p:~p:~p:~p:~p:~p:~p:~p",
                                           [Total,
                                            Processes,
                                            Processes_used,
                                            System,
                                            Atom,
                                            Atom_used,
                                            Binary,
                                            Code,
                                            Ets])),
            RRDCmd = RRDDB ++ UpdateValues,
            %io:format("~s~n",[RRDCmd]),
            os:cmd(RRDCmd);
        {badrpc, nodedown} when LoopNo > 0 ->
            io:format("~nHalt monitoring node~n"),
            erlang:halt();
        {badrpc, nodedown} when LoopNo == 0 ->
            io:format("~nWaiting for the node up~n")        
    end,    
    timer:sleep(SleepTime),
    run_memory_mon(NodeAddress, RRDDB, SleepTime, LoopNo + 1).

