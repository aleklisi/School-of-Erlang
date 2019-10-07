-module(intro_ex1).

-include_lib("eunit/include/eunit.hrl").

-export([
    hello_name/1, 
    add_three/3,
    average/1,
    fizzbuzz/0,
    thousand_sum/0]).

hello_name(Name) ->
    io:fwrite("Hello ~p!\n", [Name]).

hello() ->
    io:fwrite("Hello!\n").

% TODO uncomment following line for Ex1
% -define(Ex1, whatever).

add_three(A, B, C) ->
    ok.

% TODO uncomment following line for Ex2
% -define(Ex2, whatever).

average(_List) ->
    % TODO implement me
    ok.

% TODO uncomment following line for Ex3
% -define(Ex3, whatever).

fizzbuzz() ->
    ok.

% TODO uncomment following line for Ex3
% -define(Ex4, whatever).

thousand_sum() -> 1.

% Tests

-ifdef(Ex1).

add_three_test() ->
    ?assertEqual(0, add_three(0, 0, 0)),
    ?assertEqual(1, add_three(1, -1, 1)),
    ?assertEqual(4, add_three(2, 1, 1)),
    ?assertEqual(-3, add_three(-1, -1, -1)).

-endif.

-ifdef(Ex2).

avg([]) -> 0;
avg(List) ->
    Sum = lists:sum(List),
    Len = length(List),
    Sum / Len.

add_three_test() ->
    Cases = [
        [],
        [0],
        [1, 2, 3, 4],
        [1, 2.1, 3, 4],
        [1, -1, 3, 4]
    ],
    lists:foreach(
        fun(List) -> 
            ?assertEqual(avg(List), average(List))
        end, Cases). 
    
-endif.

-ifdef(Ex3).

fb(N) when N rem 15 == 0 -> fizzbuzz;
fb(N) when N rem 3 == 0 -> fizz;
fb(N) when N rem 5 == 0 -> buzz;
fb(N) -> N.

fizzbuzz_test() ->
    Expected = [fb(N) || N <- lists:seq(1, 100)],
    ?assertEqual(Expected, fizzbuzz()).

-endif.

-ifdef(Ex4).

thousand_sum_test() ->
    
    ?assertEqual(233168, thousand_sum()).

-endif.