-module(gcd).

-export([gcd/2]).

gcd(A, B) ->
    case_gcd(A, B). % comment this line
    % head_match_gcd(A, B). % uncomment this line for the head matching exercise

case_gcd(A, B) ->
    R = A rem B,
    case R of
        0 -> B;
        _ -> case_gcd(B, R)
    end.
