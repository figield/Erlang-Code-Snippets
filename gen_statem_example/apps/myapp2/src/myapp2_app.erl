%%%-------------------------------------------------------------------
%% @doc myapp2 public API
%% @end
%%%-------------------------------------------------------------------

-module(myapp2_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    myapp2_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
-ifdef(TEST).

simple_test() ->
    ok = application:start(myapp2),
    ?assertNot(undefined == whereis(myapp2_sup)).

-endif.