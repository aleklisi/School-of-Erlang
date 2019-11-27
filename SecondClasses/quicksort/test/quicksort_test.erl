-module(quicksort_test).

-define(MAX_INT, 10 * 1000).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

edge_cases_test() ->
    ?assertEqual([], (quicksort:sort([]))),
    ?assertEqual([1, 2], (quicksort:sort([1, 2]))),
    ?assertEqual([1, 2], (quicksort:sort([2, 1]))),
    ?assertEqual([-1, 2], (quicksort:sort([2, -1]))),
    ?assertEqual([-1, 2], (quicksort:sort([-1, 2]))),
    ?assertEqual([1], (quicksort:sort([1]))).

slightly_bigger_cases_test() ->
    sort_comparison_to_OTP_implementation(20),
    sort_comparison_to_OTP_implementation(50),
    sort_comparison_to_OTP_implementation(500),
    sort_comparison_to_OTP_implementation(5000).

sort_comparison_to_OTP_implementation(ListSize) ->
    List1 = [rand:uniform(?MAX_INT) - (?MAX_INT) / 2
	     || _ <- lists:seq(1, ListSize)],
    List1Sorted = lists:sort(List1),
    ?assertEqual(List1Sorted, (quicksort:sort(List1))).

% ===================================================================

% Quick check
quickcheck_test() ->
    ?assert((proper:quickcheck(prop_ordered(), 10 * 1000))).

prop_ordered() ->
    ?FORALL(
        L,
        list(integer()),
        ordered(quicksort:sort(L))).


%% refactor me to use tail recursion
ordered([]) -> true;
ordered([_]) -> true;
ordered([A, B | T]) -> A =< B andalso ordered([B | T]).
