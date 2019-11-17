-module(add_three_test).

-include_lib("eunit/include/eunit.hrl").

add_three_test() ->
    ?assertEqual(0, sum_three:add_three(0, 0, 0)),
    ?assertEqual(1, sum_three:add_three(1, -1, 1)),
    ?assertEqual(4, sum_three:add_three(2, 1, 1)),
    ?assertEqual(-3, sum_three:add_three(-1, -1, -1)).