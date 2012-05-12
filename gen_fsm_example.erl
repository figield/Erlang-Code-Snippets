-module(gen_fsm_example).

-behaviour(gen_fsm).

%% Interface functions
-export([start_link/1,
         start/1,
         stop/0,
         button/1,
         show_statedata/0,
         change_state/1]).

%% Callback functions
-export([init/1,
         locked/2,
         open/2,
         handle_info/3,
         handle_event/3,
         handle_sync_event/4,
         terminate/3,
         code_change/4]).

-define(FsmName, ?MODULE).
-define(Options, []).

%% Interface functions
start_link(State) ->
    gen_fsm:start_link({local, ?FsmName}, ?MODULE, State, ?Options).

%% ex: State = [1,2,3].
start(State) ->
    gen_fsm:start({local, ?FsmName}, ?MODULE, State, ?Options).

stop() ->
    gen_fsm:send_all_state_event(?FsmName, stop).

show_statedata() ->
    gen_fsm:send_all_state_event(?FsmName, show_statedata).

change_state(NewState)->
    gen_fsm:send_all_state_event(?FsmName, {change_state, NewState}).

button(Digit) ->
    gen_fsm:send_event(?FsmName, {button, Digit}).

%% Callback functions
init(Code) ->
    process_flag(trap_exit, true),
    {ok, locked, {[], Code}}.

locked({button, Digit}, {SoFar, Code}) ->
    case SoFar ++ [Digit] of
        Code ->
            io:format("Get access~n"),
            %% Do sth here!
            %% Then go to open state for 10s or unless other event will occur...
            {next_state, open, {[], Code},
             10000}; %% If an integer timeout value is provided, a timeout will occur unless an event or a message is received within Timeout milliseconds.
        Incomplete when length(Incomplete)<length(Code) ->
            io:format("Incomplete code~n"),
            {next_state, locked, {Incomplete, Code}};
        _Wrong ->
            io:format("Wrong code~n"),
            {next_state, locked, {[], Code}}
    end.

open(timeout, State) ->
    io:format("Time to close~n",[]),
    {next_state, locked, State};
open(OddEvent, State) ->
    io:format("Unexpected event has arrived in this state:~p~n",[OddEvent]),
    {next_state, locked, State}.

handle_event({change_state, NewState}, _StateName, StateData)->
    {next_state, NewState, StateData};
handle_event(show_statedata, StateName, StateData) ->
    io:format("StateName:~p~n",[StateName]),
    io:format("StateData:~p~n",[StateData]),
    {next_state, StateName, StateData};
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info({'EXIT', Pid, Reason}, _StateName, StateData) ->
    %%..code to handle exits here..
    io:format("handle msg:~p~n",[{'EXIT', Pid, Reason}]),
    {next_state, stop, StateData}.

terminate(shutdown, _StateName, _StateData) ->
    %%..code for cleaning up here..
    io:format("shutdown~n"),
    ok;
terminate(normal, _StateName, _StateData) ->
    %%..code for cleaning up here..
    io:format("Stop nromal~n"),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    %% ..code to convert state (and more) during code change
    {ok, StateName, StateData}.
