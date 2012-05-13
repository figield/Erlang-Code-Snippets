-module(temp_SUITE).

-export([all/0, 
         groups/0,
         suite/0,
         init_per_suite/1,  
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2, 
         init_per_testcase/2,  
         end_per_testcase/2]). 

-export([tc_f2c/1,
         tc_c2f/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, group1}].

groups() ->
    [{group1, 
      [],
      [tc_f2c, tc_c2f]}].

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) -> 
    ct:pal("Config:~p",[Config]),
    Cfg = ct:get_config(unix),
    ct:pal("Configuration from file:~p",[Cfg]),
    [{key, value} | Config].

end_per_suite(_Config) -> 
    ok.

init_per_group(_Group, Config) ->
    ct:pal("Config:~p",[Config]), 
    Config.

end_per_group(_Group, Config) ->
    Status = ?config(tc_group_result, Config),
    ct:pal("Status:~p", [Status]),
    ok.

init_per_testcase(_TestCase, Config) -> 
    Config.

end_per_testcase(_TestCase, _Config) -> 
    ok.

tc_f2c(_Config) ->
    ?assertEqual(0.0, temp:f2c(32)),
    ?assertEqual(0.0, temp:f2c(32.0)),
    ?assertEqual(5.0, temp:f2c(41)),
    ?assertEqual(5.0, temp:f2c(41.0)),    
    ok.

tc_c2f(_Config) ->
    ?assertEqual(41.0, temp:c2f(5)),
    ?assertEqual(41.0, temp:c2f(5.0)),
    ?assertEqual(32.0, temp:c2f(0)),
    ?assertEqual(32.0, temp:c2f(0.0)),
    ok.

%% More examples:
%% http://www.erlang.org/doc/apps/common_test/example_chapter.html
