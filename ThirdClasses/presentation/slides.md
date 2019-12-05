class: middle, center

# Processes in Erlang

School of Erlang

---
# Organizational stuff

1) [Meetup](https://www.meetup.com/pl-PL/Elixir-Krakow/events/266667008/)

2) The last classes are 10.12.2019 from 17:00 to 19:00.

---
# Erlang processes
 
 - lightweight (grow and shrink dynamically)
 
 -  small memory footprint
 
 - fast to create and terminate
 
 - the scheduling overhead is low

---
# Data handling

Data in Erlang in immutable. 

When one process passes data to the other one, data is copied between them.

Each process has its own memory and its own stack.

A number of optimizations is applied inside BEAM.

---
# How to start a new process?

Use one of following functions:

 - [spawn](http://erlang.org/doc/man/erlang.html#spawn-1)

 - [spawn_link](http://erlang.org/doc/man/erlang.html#spawn_link-1)

 - [monitor](http://erlang.org/doc/man/erlang.html#spawn_monitor-1)

---
# How to start a new process? - Example

**Live demo**

This example show how the new process is started.

```
erl
% compile example file
c(example1).

% run
example1:main().
```

---
# Monitors

When process _A_ monitors process _B_ and _B_ terminates with exit reason `Reason`, a `'DOWN'` message is sent to _A_:

```
{'DOWN', Ref, process, B, Reason}
```

Process _A_ is **NOT** terminated.

[Read more in the docs](http://erlang.org/doc/reference_manual/processes.html#monitors)

---
# Links

Two processes can be linked to each other.
A link between two processes _A_ and _B_ is created by _A_ calling the BIF `link(B)` (or conversely).
There also exist a number of `spawn_link` BIFs, which spawn and link to a process in one operation.
Links are bidirectional and there can only be one link between two processes.
Repeated calls to `link(Pid)` have no effect.

[Read more in the docs](http://erlang.org/doc/reference_manual/processes.html#links)

---
# Links - Visual Explanation

.center[ .scale450x450[![Typing disciplines diagram](https://learnyousomeerlang.com/static/img/link-exit.png)] ]

[taken from](https://learnyousomeerlang.com/errors-and-processes)

---
# Links - Linked but did not die...

When two processes are linked, and one process calls `process_flag(trap_exit, true).` and the other process dies,
even though the first process is linked to the second one it is not terminated.
It is not recommended to use this flag usually, but its good to know it exists.

---
# Processes communication

Processes communicate by sending and receiving messages.
Message sending is **asynchronous** and safe.
The message is guaranteed to eventually reach the recipient, _provided that the recipient exists_.

---
# Sending messages

Using operator `!`:

`Destination ! Msg`

where `Destination` is a PID or name of a process.

You can also use [erlang:send/2,3](http://erlang.org/doc/man/erlang.html#send-2) or
[erlang:send_after/3,4](http://erlang.org/doc/man/erlang.html#send_after-3).

[Read more in the docs](http://erlang.org/doc/reference_manual/expressions.html#send)

---
# Receiving messages

Each process has its own message queue.
If you do not match on a specific pattern it works as FIFO.

```
receive
    Pattern1 ->
        Body1;
    Pattern2 [when GuardSeq2] ->
        Body2;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

---
# Sending and receiving messages - Example

This example show how Message is send and received.

**Live demo**

```
erl
% compile example file
c(example2).

% run
example2:main().
```

---
# Selective message picking - Example

You can determine an order of messages received by matching to them.

**Live demo**

```
erl
% compile example file
c(example3).

% run
example3:main().
```

---
# How to access process?

- via PID as variable
- [register/2](http://erlang.org/doc/man/erlang.html#register-2)



---
# Behaviors

Behaviors are formalizations of these common patterns.
The idea is to divide the code for a process in a generic part
(a behavior module) and a specific part (a callback module).

They are very much like interfaces in OOP.

Examples are:

- [application](http://erlang.org/doc/design_principles/applications.html#callback_module)

- [supervisor](http://erlang.org/doc/design_principles/sup_princ.html)

- [gen_server](http://erlang.org/doc/design_principles/gen_server_concepts.html)

[Read more in the docs](http://erlang.org/doc/design_principles/des_princ.html#behaviours)

---
# How to implement a behavior - Example

```
-module(my_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    my_sup:start_link().

stop(_State) ->
    ok.
```

---
# Application structure

```
                  my_app
                    |
                    |
                   \/
                my_sup
                /    \
               /      \
              \/      \/
        another_sup   worker3
        /      \  
       /        \
      \/        \/
    worker1   worker2
```
where:

| node | behavior | 
|:---:|:---:|
| my_app | application |
| my_sup | supervisor |
| another_sup | supervisor |
| worker1 | gen_server |
| worker2 | gen_server |
| worker3 | gen_server |

---
# gen_server

 - `init` initialization
 - `handle_cast` asynchronous call
 - `handle_call` synchronous call
 - `handle_info` messages handling

[gen_server example](http://erlang.org/doc/design_principles/gen_server_concepts.html#example)
[gen_server docs](http://erlang.org/doc/man/gen_server.html)

---
### Observer

A GUI tool for observing an Erlang system.

**Live demo**

```
rebar3 shell
observer:start().
```
---
# Read more:
 
 - [Processes](http://erlang.org/doc/efficiency_guide/processes.html)

 - [Concurrent Programming](http://erlang.org/doc/getting_started/conc_prog.html)

 - [Intro to Concurrency](https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency)
