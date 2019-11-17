-module(average_test).

-include_lib("eunit/include/eunit.hrl").

avg([]) -> 0;
avg(List) ->
    Sum = lists:sum(List),
    Len = length(List),
    Sum / Len.

average_of_empty_list_is_zero_test() ->
    List = [],
    ?assertEqual(avg(List), average:average(List)).

average_of_zeros_is_zero_test() ->
    List = [0],
    ?assertEqual(avg(List), average:average(List)),
    List2 = [0, 0, 0, 0],
    ?assertEqual(avg(List2), average:average(List2)).

average_works_with_natural_numbers_test() ->
    List = [1, 2, 3, 4],
    ?assertEqual(avg(List), average:average(List)).

average_with_a_floats_test() ->
    List = [1, 2.1, 3, 4],
    ?assertEqual(avg(List), average:average(List)).

average_with_negative_numbers_test() ->
    List = [1, -1, 3, 4],
    ?assertEqual(avg(List), average:average(List)).

