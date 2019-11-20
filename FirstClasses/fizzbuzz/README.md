## Ex 3 - FizzBuzz

Write a function that returns the numbers from 1 to 100. But for multiples of three replaces a number with an atom `fizz` instead of the number and for the multiples of five replaces a number with an atom `buzz`. For numbers which are multiples of both three and five replaces a number with an atom `fizzbuzz`.

Expected result should me something like this:

```erlang
[1,2,fizz,4,buzz,fizz]
```

Notice that for longer output we get if shortened to:

```erlang
5> fizzbuzz:fizzbuzz().
[1,2,fizz,4,buzz,fizz,7,8,fizz,buzz,11,fizz,13,14,fizzbuzz,
 16,17,fizz,19,buzz,fizz,22,23,fizz,buzz,26,fizz,28,29|...]
```
To print whole output use:

```erlang
6> rp(fizzbuzz:fizzbuzz()).
[1,2,fizz,4,buzz,fizz,7,8,fizz,buzz,11,fizz,13,14,fizzbuzz,
 16,17,fizz,19,buzz,fizz,22,23,fizz,buzz,26,fizz,28,29,
 fizzbuzz,31,32,fizz,34,buzz,fizz,37,38,fizz,buzz,41,fizz,43,
 44,fizzbuzz,46,47,fizz,49,buzz]
```

Try different implementations:
 - Using function [Pattern Matching](http://erlang.org/doc/reference_manual/patterns.html)
 - Using [`case of .. end`](http://erlang.org/doc/reference_manual/expressions.html#case) construction.

### Tips

Try writing a function to determine output over a single number, and than generate a list `[1, 2, 3, 4, ..., 100]` and map the function over a generated list.

Useful functions:
 - [generating a list sequence](http://erlang.org/doc/man/lists.html#seq-2)
 - [map function](http://erlang.org/doc/man/lists.html#map-2)
 - [alternatively to maps use list comprehension](http://erlang.org/doc/programming_examples/list_comprehensions.html)
