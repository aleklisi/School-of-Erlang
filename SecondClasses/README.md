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

Last but not least we can extracting an anonymous function to a named one:

```erlang
sum(List) ->
    lists:foldl(
        fun add/2,
        0,
        List).

add(A, B) ->
    A + B.
```
