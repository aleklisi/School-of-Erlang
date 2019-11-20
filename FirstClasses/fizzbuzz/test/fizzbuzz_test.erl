-module(fizzbuzz_test).

-include_lib("eunit/include/eunit.hrl").

fb(N) when N rem 15 == 0 -> fizzbuzz;
fb(N) when N rem 3 == 0 -> fizz;
fb(N) when N rem 5 == 0 -> buzz;
fb(N) -> N.

fizzbuzz_small_test() ->
    Expected = [fb(N) || N <- lists:seq(1, 6)],
    io:fwrite("~p", [Expected]),
    ?assertEqual(Expected, fizzbuzz:fizzbuzz()).

fizzbuzz_big_test() ->
    Expected = [fb(N) || N <- lists:seq(1, 100)],
    ?assertEqual(Expected, fizzbuzz:fizzbuzz()).