-module(rev_polish_notation_test).

-include_lib("eunit/include/eunit.hrl").

evaluator_positive_test() ->
    ?assertEqual(7, rev_polish_notation:evaluate([ 3, 4, fun erlang:'+'/2 ])),
    ?assertEqual(17, rev_polish_notation:evaluate([ 3, 4, 5, fun erlang:'*'/2, fun erlang:'-'/2 ])),
    ?assertEqual(5, rev_polish_notation:evaluate([ 3, 4, fun erlang:'-'/2, 5, fun erlang:'*'/2 ])).

evaluator_negative_test() ->
    ?assertEqual(1, 1).