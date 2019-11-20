-module(add_three_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

add_three_test() ->
    ?assertEqual(0, sum_three:add_three(0, 0, 0)),
    ?assertEqual(1, sum_three:add_three(1, -1, 1)),
    ?assertEqual(4, sum_three:add_three(2, 1, 1)),
    ?assertEqual(-3, sum_three:add_three(-1, -1, -1)).

add_three_proper_test() ->
    Property = ?FORALL(
        {A, B, C},
        {integer(), integer(), integer()},
        lists:sum([A, B, C]) =:= sum_three:add_three(A, B, C)),
    ?assert(proper:quickcheck(Property, 100000)).
