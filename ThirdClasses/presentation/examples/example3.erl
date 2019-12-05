-module(example3).

-export([main/0]).

main() ->
    ReceiverPid = spawn(fun receiver/0),
    print(ReceiverPid),
    ReceiverPid ! hi,
    ReceiverPid ! hey,
    ReceiverPid ! hello,
    ok.

receiver() ->
    print(process_info(self(), message_queue_len)),
    receive
        Msg1 = hello ->
            print(Msg1);
        Msg2 = hi ->
            print(Msg2);
        Msg3 ->
            print(Msg3)
    end,
    receiver().
            

print(Term) ->
    io:fwrite("My Pid is ~p and I want to print: ~p\n", [self(), Term]).
