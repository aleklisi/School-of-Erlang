-module(example_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
	  init_per_suite/1, end_per_suite/1,
      init_per_group/2, end_per_group/2,
	  init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
   my_first_test/1,
   first_in_a_parallel_group/1,
   second_in_a_parallel_group/1
]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [].

init_per_suite(Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    Config.

end_per_suite(_Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ok.

init_per_group(_GroupName, Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    Config.

end_per_group(_GroupName, _Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ok.

init_per_testcase(_Case, Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    Config.

end_per_testcase(_Case, _Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ok.

groups() ->
    [
        {example_group_name, [parallel], [
            first_in_a_parallel_group,
            second_in_a_parallel_group
        ]}
    ].

all() ->
   [
        my_first_test,
        {group, example_group_name}

   ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

my_first_test(Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ct:log("This is how we log stuff in the ct and this is the Config: ~p", [Config]).

first_in_a_parallel_group(_Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ok.

second_in_a_parallel_group(_Config) ->
    ct:log("I am in ~p:~p\n", [?MODULE, ?FUNCTION_NAME]),
    ok.
