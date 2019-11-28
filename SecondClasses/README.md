# Syntax improvements

**Make it work, then make it beautiful, then if you really, really have to, make it fast.**

_Joe Armstrong_

## Refactoring

### Implement a function so that is works

The original implementation using `case ... of ... end`:

```erlang
sum(List = [Head | Tail]) ->
    case List of
        [] ->
            0;
        _ -> 
            Head + sum(Tail)
    end.
```

### Matching function head

Erlang allows to have more than one function head:

```erlang
sum([]) -> 0;
sum([Head | Tail]) ->
    Head + sum(Tail).
```

### Use tail recursion

Now lets take the sum function and (if possible) make it _tail recursive_:

```erlang
% Add a wrapper function to keep the API compatibility
sum(List) ->
    sum(List, 0).

sum([], Sum) -> Sum;
sum([Head | Tail], Sum) ->
    sum(Tail, Sum + Head).
```

### Use generic function to express your problem

Next we can use generic [lists:foldl/3](http://erlang.org/doc/man/lists.html#foldl-3) for handling this:

```erlang
sum(List) ->
    lists:foldl(
        fun(Elem, Acc) ->
            Elem + Acc
        end,
        0,
        List).
```

Moreover we can extracting an anonymous function to a named one:

```erlang
sum(List) ->
    lists:foldl(
        fun add/2,
        0,
        List).

add(A, B) ->
    A + B.
```

Last but not least we can use already defined function in erlang:

```erlang
sum(List) ->
    lists:foldl(
        fun erlang:'+'/2,
        0,
        List).
```

## List comprehension

A list comprehension is a syntactic construct available in some programming languages for creating a list based on existing lists.

```erlang

OutsideVariable

[ Result || Generator or Generators , Filter ]

```

### Examples

#### Reverse all elements

```erlang
1> List = [1, 5, 2, 0, 3, 0.0, 1.2 ].
[1,5,2,0,3,0.0,1.2]

2> [ 1 / X || X <- List ].
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 1 / 0

3> [ 1 / X || X <- List, X /= 0 ].
[1.0,0.2,0.5,0.3333333333333333,0.8333333333333334]
```

Lets notice we can cannot divide by 0, so we may want to filter zeros out from the list.

#### Filter only even numbers

```erlang
1> List = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].
[1,2,3,4,5,6,7,8,9,10]
2> [ X || X <- List, X rem 2 == 0 ].
[2,4,6,8,10]
```

## Twin primes

A twin prime is a prime number that is either 2 less or 2 more than another prime number—for example, either member of the twin prime pair (41, 43). 

```erlang
1> Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97].
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,
 79,83,89,97]

2> [{X, Y} || X <- Primes, Y <- Primes, X + 2 == Y].
[{3,5},
 {5,7},
 {11,13},
 {17,19},
 {29,31},
 {41,43},
 {59,61},
 {71,73}]
```

[List Comprehensions docs](https://erlang.org/doc/programming_examples/list_comprehensions.html)

## Debug vs Tracing

### Debugger

Lets start BEAM with the project modules loaded: `rebar3 shell`.

```erlang
% A module needs to be compiled with `debug_info` flag.
1> c(list_based_auth_service, debug_info).
Recompiling /Users/aleksanderlisiecki/Documents/SchoolOfErlang/SecondClasses/auth/src/list_based_auth_service.erl
{ok,list_based_auth_service}
% Lets start a debugger.
2> debugger:start().
{ok,<0.179.0>}
```

New window will be opened.

Go to _Module_ -> _Interpret..._ -> pick a file you want to debug -> press _OK_.

Go to _Break_ -> _Line Break..._ -> choose a module to debug from a list -> type in a line -> press _OK_.

Now back to the `erl` console:

```erlang
% We create a state 
3> S1 = list_based_auth_service:add_user("Alek", "Pass", []).
[{"Alek","Pass"}]
4> list_based_auth_service:user_exists("Alek", S1).
% Go to the debugger window now
true
5> list_based_auth_service:user_exists("Basia", S1).
% Go to the debugger window now
false
```

Just the commands:

```erlang
c(list_based_auth_service, debug_info).
debugger:start().
S1 = list_based_auth_service:add_user("Alek", "Pass", []).
list_based_auth_service:user_exists("Alek", S1).
list_based_auth_service:user_exists("Basia", S1).
```


[debugger docs](http://erlang.org/doc/apps/debugger/debugger_chapter.html)


### Tracing

#### DBG

```erlang
dbg:tracer().
dbg:p(all, c).
dbg:tpl(list_based_auth_service, user_exists, x).
dbg:tpl(list_based_auth_service, add_user, x).

dbg:stop_clear().
```

#### Erlang Doctor
```erlang

P="/tmp/tr.erl",ssl:start(), inets:start(), {ok, {{_, 200, _}, _, Src}} = httpc:request("https://git.io/fj024"), file:write_file(P, Src), {ok, tr, B} = compile:file(P, binary), code:load_binary(tr, P, B), rr(tr), tr:start().

tr:trace_calls([list_based_auth_service]).
S1 = list_based_auth_service:add_user("Alek", "Pass", []).
list_based_auth_service:user_exists("Alek", S1).
list_based_auth_service:user_exists("Basia", S1).
tr:stop_tracing_calls().
tr:select().
```

Tracing tools:
 - [dbg](http://erlang.org/doc/man/dbg.html)
 - [recon](https://github.com/ferd/recon)
 - [erlang_doctor](https://github.com/chrzaszcz/erlang_doctor)