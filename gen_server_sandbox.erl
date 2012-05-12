%%% ----------------------------------------------------------
%%% #1.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% Author:	 Dawid Figiel
%%% Description: gen_server sandbox
%%%
%%% Change the code run it and play again ;)
%%%
%%% gen_server_example:start(),
%%% gen_server_example:show_statistics().
%%% gen_server_example:populate2(40000).
%%%
%%% ----------------------------------------------------------
-module(gen_server_sandbox).
-vsn('0.01').

-behaviour(gen_server).

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% API funs
-export([ show_statistics/0,
          populate/1,
          populate2/1
        ]).

%% API funs for the gen_server
-export([ start_link/0,
          stop/0
        ]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%% gen_server funs
-export([ init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

%% Test funs
-export([ start/0
        ]).

%%% ----------------------------------------------------------
%%% #2.3   INCLUDES
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #2.4   DEFINES
%%% ----------------------------------------------------------

-define(CALL_TMO, 60000).  %% millisecs, 1 min
-define(LIVETIME, 180000).  %% millisecs, 3 min

-record(state,
        { 
          population_size = 0,
          start_heap_size = 0,
          start_memory = 0,
          one_proc_memory = 0,
          one_proc_heap_size = 0
        }).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% ----------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

show_statistics() ->
    gen_server:call(?MODULE, show_statistics, ?CALL_TMO).

populate(Num) ->
    gen_server:call(?MODULE, {populate, Num}, ?CALL_TMO).

populate2(Num) ->
    populate2_loop(0, Num).

populate2_loop(Num, Num) ->
    ok;
populate2_loop(Counter, Num) ->
    io:format("Counter:~p ~n",[Counter]),
    case catch gen_server:call(?MODULE, {give_one_birth, Counter + 1}, ?CALL_TMO) of
        ok ->
            populate2_loop(Counter + 1, Num);
        {'EXIT', Reason} ->
            io:format("~n~n~nExit reason:~p~n~n",[Reason]);
        E ->
            io:format("~nStop reason:~p~n",[E])
    end.

%%--------------------------------------------------------------------
%% @spec start() -> Reply
%%
%% where 
%%      Reply = {ok, Pid} | ignore | {error, Error}
%%
%% @doc
%%     Creates a gen_server process which 
%%     calls Module:init/1 to initialize. To ensure a synchronized 
%%     start-up procedure, this function does not return until Module:init/1 
%%     has returned.
%%
%% @end
%%--------------------------------------------------------------------
start() -> 
    gen_server:start({local,?MODULE},?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec start_link() -> Reply
%%
%% @doc
%%     Same functionatily with the start/0 except that it links 
%%     to the process.
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec  stop() -> Reply
%%
%% where
%%      Reply = ok | {error, Error}
%%      Error = error()
%%
%% @doc
%%     Stop the gen_server.
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE,stop).


%%% ----------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec Module:init([]) -> Result
%%
%% where
%%      Result = {ok,State} 
%%               | {ok,State,Timeout} 
%%               | {ok,State,hibernate}
%%               | {stop,Reason} 
%%               | ignore
%%      State = term()
%%      Timeout = int()>=0 | infinity
%%      Reason = term()
%%
%% @doc
%%     Start gen_server with default state.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %process_flag(trap_exit, true),
    {M0, H0} = lists:foldl(fun(P, {Memory, Heap}) ->
                                   {heap_size, H} = erlang:process_info(P, heap_size),
                                   {memory, M} = erlang:process_info(P, memory),
                                   {M + Memory, H + Heap}
                           end, {0,0}, erlang:processes()),                       
    {ok, #state{start_heap_size = H0,
                start_memory = M0}}.

%%--------------------------------------------------------------------
%% @spec Module:handle_call(Request, From, State) -> Result
%%
%% where
%%      Request = term()
%%      From = {pid(),Tag}
%%      State = term()
%%      Result = {reply,Reply,NewState} | {reply,Reply,NewState,Timeout}
%%               | {reply,Reply,NewState,hibernate}
%%               | {noreply,NewState} | {noreply,NewState,Timeout}
%%               | {noreply,NewState,hibernate}
%%               | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%%      Reply = term()
%%      NewState = term()
%%      Timeout = int()>=0 | infinity
%%      Reason = term()
%%
%% @doc
%%     Gen_server handle call functions.
%%
%% @end
%%--------------------------------------------------------------------

handle_call({populate, Num}, _From, S) ->
    populate_loop(0, Num),
    {reply, ok, S};

handle_call({give_one_birth, 1}, _From, S) ->
    Pid = spawn(fun()->
                        receive after ?LIVETIME -> ok end
                end),
    {heap_size, H} = erlang:process_info(Pid, heap_size),
    {memory, M} = erlang:process_info(Pid, memory),
    {reply, ok,  S#state{population_size = S#state.population_size + 1,
                         one_proc_memory = M,
                         one_proc_heap_size = H
                        }};

%% handle_call({give_one_birth, Num}, _From, S) ->
%%     case catch spawn(fun()-> receive after ?LIVETIME -> ok end end) of
%%         Pid when is_pid(Pid) ->
%%             case Num rem 100 of
%%                 0 -> io:format("~p ", [Num]);
%%                 _ -> ok
%%             end,
%%             {reply, ok, S#state{population_size = S#state.population_size + 1}};
%%         EE ->
%%             io:format("~n~nEE:~p~n~n", [EE]),
%%             {reply, error, S}
%%     end;

handle_call({give_one_birth, Num}, _From, S) ->
    spawn(fun()-> receive after ?LIVETIME -> ok end end),
    case Num rem 100 of
        0 -> io:format("~p ", [Num]);
        _ -> ok
    end,
    {reply, ok, S#state{population_size = S#state.population_size + 1}};
    

handle_call(show_statistics, _From, S) ->
    ProcMemorySum = S#state.population_size * S#state.one_proc_memory,
    ProcHeapSizeSum = S#state.population_size * S#state.one_proc_heap_size,
    io:format("Memory usage of the population(~p):~p Bytes~n",[S#state.population_size, ProcMemorySum]),
    io:format("Heap size of the population:~p words~n",[ProcHeapSizeSum]),
    io:format("Total memory usage of all processes(~p):~p Bytes~n",[length(erlang:processes()), ProcMemorySum + S#state.start_memory]),
    io:format("Total heap size of of all processes:~p words~n",[ProcHeapSizeSum + S#state.start_heap_size]),
    {reply, ok, S};

handle_call(stop, _From, _S) ->
    {stop, normal, manually_stopped, #state{}};

handle_call(_, _From, S) ->
    {reply, ok, S}.

%%--------------------------------------------------------------------
%% @spec Module:handle_cast(Request, State) -> Result
%%
%% where
%%      Request = term()
%%      State = term()
%%      Result = {noreply,NewState} 
%%               | {noreply,NewState,Timeout}
%%               | {noreply,NewState,hibernate}
%%               | {stop,Reason,NewState}
%%      NewState = term()
%%      Timeout = int()>=0 | infinity
%%      Reason = term()
%%
%% @doc
%%     Gen_server callback function
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec Module:handle_info(Info, State) -> Result
%%
%% where
%%      Info = timeout | term()
%%      State = term()
%%      Result = NewState | Timeout | Reason
%%      NewState = term()
%%      Timeout = int()>=0 | infinity
%%      Reason = normal | term()
%%
%% @doc
%%     Gen_server callback function
%%
%% @end
%%--------------------------------------------------------------------
handle_info({give_one_birth, 1}, S) ->
    Pid = spawn(fun()->
                        receive after ?LIVETIME -> ok end
                end),
    {heap_size, H} = erlang:process_info(Pid, heap_size),
    {memory, M} = erlang:process_info(Pid, memory),
    {noreply, S#state{population_size = S#state.population_size + 1,
                      one_proc_memory = M,
                      one_proc_heap_size = H
                     }};

handle_info({give_one_birth, Num}, S) ->
    spawn(fun()->
                  receive after ?LIVETIME -> ok end
          end),
    case Num rem 100 of
        0 -> io:format("~p ", [Num]);
        _ -> ok
    end,           
    {noreply, S#state{population_size = S#state.population_size + 1}};

handle_info({'EXIT', _Pid, normal}, S) ->
    {noreply, S#state{population_size = S#state.population_size - 1}};

handle_info({'EXIT', Pid, Reason}, S) ->
    io:format("~p: Died beacause of ~p!~n",[Pid, Reason]),
    {noreply, S#state{population_size = S#state.population_size - 1}}.

%%--------------------------------------------------------------------
%% @spec Module:terminate(Reason, State)
%%
%% where
%%      Reason = normal | shutdown | {shutdown,term()} | term()
%%      State = term()
%%
%% @doc
%%     Consider to do some cleanings here.
%%
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) -> 
    io:format("~nTerminated because of: ~p~n",[Reason]).

%%--------------------------------------------------------------------
%% @spec Module:code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% where
%%      OldVsn = Vsn | {down, Vsn}
%%      Vsn = term()
%%      State = NewState = term()
%%      Extra = term()
%%
%% @doc
%%     Gen_server code_change function
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}. 

%%% ----------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

populate_loop(Num, Num) ->
    ok;
populate_loop(Counter, Num) ->
    self() ! {give_one_birth, Counter + 1},
    populate_loop(Counter + 1, Num).

%%
%% Test funs
%%



