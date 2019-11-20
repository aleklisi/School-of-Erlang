-module(fibonacci_sum_test).

-include_lib("eunit/include/eunit.hrl").

fibonacci_sum_test() ->
    ?assertEqual(4613732, fibonacci_sum:fibonacci_sum()).

fibonacci_sum_small_test() ->
    ?assertEqual(44, fibonacci_sum:fibonacci_sum(1, 1, 0, 100)).
