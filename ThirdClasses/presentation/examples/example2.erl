-module(example2).

-export([main/0]).

main() ->
    ReceiverPid = spawn(fun receiver/0),
    spawn(fun() -> sender(ReceiverPid) end),
    ok.

receiver() ->
    receive
        Msg ->
            print(Msg)
    after
        3000 ->
            print("No Msg!")
    end.
            
sender(ToPid) ->
    print(ToPid),
    Msg = "Hello",
    print(Msg).
    % ToPid ! Msg.

print(Term) ->
    io:fwrite("My Pid is ~p and I want to print: ~p\n", [self(), Term]).