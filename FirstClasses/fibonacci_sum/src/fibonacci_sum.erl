-module(fibonacci_sum).

-export([fibonacci_sum/0, fibonacci_sum/4]).

fibonacci_sum() ->
    Limit = 4 * 1000 * 1000,
    fibonacci_sum(1, 1, 0, Limit).

fibonacci_sum(_Prev, _Curr, _Sum, _Limit) ->
    1.
