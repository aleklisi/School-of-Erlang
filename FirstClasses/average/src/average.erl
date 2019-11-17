-module(average).

-export([average/1]).
-export([start_with/1]).

average(_List) ->
    ok.

start_with([]) ->
    is_empty_string;
start_with([$A | _]) ->
    starts_with_A;
start_with([_ | _]) ->
    starts_with_something_else.