-module(vertical_tree_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%% Test server callbacks
-export([suite/0, all/0,
	  init_per_suite/1, end_per_suite/1,
	  init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
   vertical_tree/1
]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    application:ensure_all_started(supervion_trees),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

all() ->
   [
        vertical_tree
   ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

vertical_tree(_Config) ->
    TopSupChildren = supervisor:which_children(top_sup),
    ?assertEqual(1, length(TopSupChildren)),

    ASupChildren = supervisor:which_children(a_sup),
    ?assertEqual(1, length(ASupChildren)),

    BSupChildren = supervisor:which_children(b_sup),
    ?assertEqual(1, length(BSupChildren)),

    CSupChildren = supervisor:which_children(c_sup),
    ?assertEqual(0, length(CSupChildren)).
