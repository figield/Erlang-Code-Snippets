-module(pushbutton).
-behaviour(gen_statem).

-export([start/0, start_link/0, push/0, get_count/0, get_dbcontent/0, stop/0]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([on/3, off/3]).

name() -> pushbutton_statem. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.

start_link() ->
  gen_statem:start_link({local, name()}, ?MODULE, [], []).

start() ->
  gen_statem:start({local, name()}, ?MODULE, [], []).

push() ->
  gen_statem:call(name(), push).

get_count() ->
  gen_statem:call(name(), get_count).

get_dbcontent() ->
  gen_statem:call(name(), get_dbcontent).

stop() ->
  gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

init([]) ->
  %% Set the initial state + data.  Data is used only as a counter.
  State = off,
  Counter = 0,
  {ok, RedisClientHandler} = eredis:start_link(),
  {ok, State, {Counter, RedisClientHandler}}.
callback_mode() -> state_functions.

%%% State function(s)

off({call, From}, push, {Counter, RedisClientHandler}) ->
  %% Go to 'on', increment count and reply
  %% that the resulting status is 'on'

  {ok, <<"OK">>} = eredis:q(RedisClientHandler, ["SET", "counter", Counter + 1]),

  {next_state, on, {Counter + 1, RedisClientHandler}, [{reply, From, on}]};

off(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

on({call, From}, push, Data) ->
  %% Go to 'off' and reply that the resulting status is 'off'
  {next_state, off, Data, [{reply, From, off}]};

on(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call, From}, get_count, Data) ->
  %% Reply with the current count
  {keep_state, Data, [{reply, From, Data}]};

handle_event({call, From}, get_dbcontent, Data = {_Counter, RedisClientHandler}) ->
  %% Reply with the current count

  {ok, R} = eredis:q(RedisClientHandler, ["GET", "counter"]),
  %R = 0,

  {keep_state, Data, [{reply, From, R}]};

handle_event(_, _, Data) ->
  %% Ignore all other events
  {keep_state, Data}.

