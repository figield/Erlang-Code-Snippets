%%%-------------------------------------------------------------------
%% @doc myapp2 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(myapp2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%init([]) ->
%    {ok, { {one_for_all, 0, 1}, []} }.


init2([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(pushbutton, worker)]} }.

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [#{id => pushbutton,
                    start => {pushbutton, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [pushbutton]}],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
