-module(example1).

-export([main/0]).

main() ->
    print_my_pid(),
    spawn(fun print_my_pid/0),
    print_my_pid().

print_my_pid() ->
    io:fwrite("My PID is ~p\n", [self()]).
