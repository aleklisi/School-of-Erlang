# Reverse Polish notation

## Description

In reverse Polish notation, the operators follow their operands; for instance, to add 3 and 4, one would write 3 4 + rather than 3 + 4. If there are multiple operations, operators are given immediately after their second operands; so the expression written 3 − 4 + 5 in conventional notation would be written 3 4 − 5 + in reverse Polish notation: 4 is first subtracted from 3, then 5 is added to it. An advantage of reverse Polish notation is that it removes the need for parentheses that are required by infix notation. While 3 − 4 × 5 can also be written 3 − (4 × 5), that means something quite different from (3 − 4) × 5. In reverse Polish notation, the former could be written 3 4 5 × −, which unambiguously means 3 (4 5 ×) − which reduces to 3 20 − (which can further be reduced to -17); the latter could be written 3 4 − 5 × (or 5 3 4 − ×, if keeping similar formatting), which unambiguously means (3 4 −) 5 ×.

[https://en.wikipedia.org/wiki/Reverse_Polish_notation](Read more here)

Try writing a calculator that takes a list or arguments and functions and evaluates them following the _reverse Polish notation_.
For simplicity sake lets assume that only following arguments are allowed:
 - Functions:
   - `+`, 
   - `-`, 
   - `*`, 
   - `/`, 
   - `rem`, 
   - `fun(X) -> X div 2 end`, 
   - `fun(X) -> X - 1 end`
 - Numbers:
   - Integers
   - Floats

Maybe some of the following observations will be helpful:
```erlang
1> erlang:'+'(1, 2).
3
2> erlang:'rem'(5, 2).
1
3> erlang:apply(fun erlang:'rem'/2, [5, 3]).
2
```

The input examples:


| Postfix notation| Infix notation |  Result |
|---|---|---|
| `[ 3, 4, fun erlang:'+'/2 ]` | `3 + 4` | `7` |
| `[ 3, 4, 5, fun erlang:'*'/2, fun erlang:'-'/2 ]` |  `(5 * 4) - 3` | `17` |
| `[ 3, 4, fun erlang:'-'/2, 5, fun erlang:'*'/2 ]` |  `(4 - 3) * 5` |  `5` |



## Task 1 - tail recursion

Try using the same idea as _tail recursion_ , storing the stack as a result.

## Task 2 - foldl

Try using `lists:foldl/3`.
Maybe it would be helpful to use named function instead of anonymous passed ti the fold.

## Task 3 - error and error handling

Propose error handling mechanism.
How about returning `{error, Reason}`?
What happens if we have 2 elements on a stack but no more operations to run?
What happens when we have a function to execute but no arguments on a stack.

## Tips

- First implement the **happy path** and only then be concerned about the error handling.
- Use _guards_ with many function heads.
- [erlang:is_function/2](http://erlang.org/doc/man/erlang.html#is_function-2) might be helpful inside guards.
- Notice that the only allowed functions are of /1 or /2, so maybe it is worth separating those to cases?
- You can use a list as a stack by adding the element ad the top and later in removing it:

```erlang

%These are just wrappers for easier understanding:

init_stack() -> [].

push(List, Elem) ->
    [Elem | List].

% No empty stack handling included
pop([Elem | Tail]) ->
    {Elem, Tail}.

% No empty or 1 element stack handling included
pop2([E1, E2 | T]) ->
    {E1, E2, T}.

% Console output:

Stack0 = init_stack().                  % []
Stack1 = push(Stack0, a).               % [a]
Stack2 = push(Stack1, b).               % [b, a]
Stack3 = push(Stack2, c).               % [c, b, a]
{PoppedElem1, Stack4} = pop(Stack3).    % {c, [b, a]}
{PoppedElem2, Stack5 = pop(Stack4).     % {b, [a]}
Stack6 = push(Stack5, d).               % [d, a]
Stack7 = push(Stack6, e).               % [e, d, a]
Stack8 = pop2(Stack7).                  % {e, d, [a]}
```
