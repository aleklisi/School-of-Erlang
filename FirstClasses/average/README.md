## Ex 2 - Average

Try to implement a function that takes a list and returns average of its elements. Average is a sum of elements divided by their amount. Start with splitting problem into smaller parts: summing elements of a list, and finding list length.

### Sum

Implement a function `sum/1`, consider two cases:
 - sum of elements of empty list `[]` is `0`;
 - sum of nonempty list is a sum of head of a list summed with a sum of tail of a list.

#### Tips:

Lists in Erlang can be matched and split to head and tail:

```erlang
% When:
List = [1, 2, 3, 4],
[Head | Tail] = List.
% Than:
> Head.
1
> Tail.
[2, 3, 4]
```

Pattern matching can be useful:

```erlang
-module(m).

-export([m/1]).

start_with([]) ->
    is_empty_string;
start_with([$A | _]) ->
    starts_with_A;
start_with([_ | _]) ->
    starts_with_something_else.
```

Notice that `;` is used to separate cases from each other. Moreover cases are matched top to bottom, so order of the cases does matter. To see that switch places between second and third case, `starts_with_A` will never be returned.

```erlang
1> c(m).
{ok,m}
2> m:start_with("").
is_empty_string
3> m:start_with("Alek").
starts_with_A
4> m:start_with("Basia").
starts_with_something_else
```

Congratulations, you've just managed to implement you first function :)

### Length

Notice that length is almost the same function as sum, but instead of adding the `Head` value you just need to add `1`.

#### Tips

For length you can use the function that already exists, see: [length](http://erlang.org/doc/man/erlang.html#length-1)

### Next steps

 - Try implementing sum function with:
  - tail recursion see: [here](https://stackoverflow.com/questions/33923/what-is-tail-recursion)
  - use one of fold functions see: [foldl](http://erlang.org/doc/man/lists.html#foldl-3) or [foldr](http://erlang.org/doc/man/lists.html#foldr-3)
