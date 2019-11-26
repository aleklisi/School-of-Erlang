-module(gcd_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual(4, gcd:gcd(4, 16)),
    ?assertEqual(4, gcd:gcd(16, 4)),
    ?assertEqual(15, gcd:gcd(15, 60)),
    ?assertEqual(5, gcd:gcd(15, 65)),
    ?assertEqual(4, gcd:gcd(1052, 52)).
