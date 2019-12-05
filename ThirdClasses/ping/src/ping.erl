-module(ping).

-include_lib("eunit/include/eunit.hrl").

% API
-export([ping_all/5]).

% types
-type byte_integer() :: 0..255.
-type maybe_range() :: {byte_integer(), byte_integer()} | byte_integer().


-spec ping_all(FirstR, SecondR, ThirdR, FourthR, Count) -> ok when 
    FirstR :: maybe_range(),
    SecondR :: maybe_range(),
    ThirdR :: maybe_range(),
    FourthR :: maybe_range(),
    Count :: pos_integer().
ping_all(MaybeRange1, MaybeRange2, MaybeRange3, MaybeRange4, Count) ->
    % 1) replace mabye_ranges = {From, To} with list [Form, To] and  
    % and replace mabye_ranges = SingleInt with lists [SingleList] 
    % 2) generate a list of IP addresses, a list comprehension might be helpful
    % 3) foreach IP address spawn a process which will execute ping command and then
    % it will send a result to the parent process
    % 4) Parent process should receive messages in a loop 
    %  when all processes finish their tasks and report the results print the result.
    ok.

make_range(_) -> ok.

% ping(StringIpAddress, Count) ->
%     Cmd = "ping -c " ++ Count ++ " " ++ StringIpAddress,
%     os:cmd(Cmd).

parse_result_form_ping_cmd(_) ->
    % Maybe use list matching and recursion matching first few element??
    % checkout https://erlang.org/doc/man/string.html
    ok.


%% ===================================================================================
%% Unit tests
%% ===================================================================================

%% making ranges test
integer_is_replaced_with_single_element_list_test() ->
    ?assertEqual([5], make_range(5)),
    ?assertEqual([-1], make_range(-1)),
    ?assertEqual([1], make_range(1)).

integer_is_replaced_with_single_element_list_with_random_int_test() ->
    [
        begin 
            RandomInt = rand:uniform(2000 * 1000) - 1000 * 1000,
            ?assertEqual([RandomInt], make_range(RandomInt))
        end || _ <- lists:seq(1, 1000 * 10) ].

range_is_replaced_with_a_list_starting_with_from_and_ending_with_to_test() ->
    Range = make_range({5, 255}),
    ?assertEqual(5, hd(Range)),
    ?assertEqual(10, lists:last(Range)).

one_element_range_works_test() ->
    Range = make_range({2, 2}),
    ?assertEqual([2], Range).

fail_ping_result() ->
    "PING 192.168.0.2 (192.168.0.2): 56 data bytes\n"
    "Request timeout for icmp_seq 0\n"
    "Request timeout for icmp_seq 1\n"
    "Request timeout for icmp_seq 2\n"
    "\n"
    "--- 192.168.0.2 ping statistics ---\n"
    "4 packets transmitted, 0 packets received, 100.0% packet loss\n".

successful_ping_result() ->
    "PING 192.168.43.179 (192.168.43.179): 56 data bytes\n"
    "\n"
    "64 bytes from 192.168.43.179: icmp_seq=0 ttl=64 time=0.439 ms\n"
    "64 bytes from 192.168.43.179: icmp_seq=1 ttl=64 time=0.093 ms\n"
    "64 bytes from 192.168.43.179: icmp_seq=2 ttl=64 time=0.091 ms\n"
    "64 bytes from 192.168.43.179: icmp_seq=3 ttl=64 time=0.088 ms\n"
    "\n"
    "--- 192.168.43.179 ping statistics ---\n"
    "4 packets transmitted, 4 packets received, 0.0% packet loss\n"
    "round-trip min/avg/max/stddev = 0.088/0.178/0.439/0.151 ms\n".

parisng_failed_ping_result_test() ->
    ParseResult = parse_result_form_ping_cmd(fail_ping_result()),
    ?assertEqual(0, ParseResult).

parisng_succesfull_ping_result_test() ->
    ParseResult = parse_result_form_ping_cmd(successful_ping_result()),
    ?assertEqual(0, ParseResult).