# Syntax introduction

### Ex 0 - Compile module

- Open file `intro_ex1.erl`.
- Open terminal and go to: `$REPO/Introduction`.
- Run `erl` (BEAM) in terminal. Notice the prompt should change to command counter: `1>`.
- To compile a module type to terminal `c(intro_ex1).`, do not forget about the dot (`.`) at the end of expression.
- Check the result, it should look like `{ok, intro_ex1}`.
- Try calling the function `hello_name/1` with your name as string argument: `intro_ex1:hello_name("Aleksander").`

```erlang 
Hello "Aleksander"!
ok
```

should be printed to terminal. Well done, welcome to erlang world :)

- Notice that `intro_ex1.beam` file was created next to `intro_ex1.erl`, it is BEAM binary file.

#### Tips

 - Notice the directive `-module(intro_ex1).` in the first line, it must be the same as filename without extension.
 - Remember to export all of the functions you want to have access to outside module with `-export([foo/2]).`

## Ex 1 - Sum three

- Go to file `intro_ex1.erl` and find `add_three/3` function.
- Open `erl` in terminal, uncomment line `% -define(Ex1, whatever).` and compile `intro_ex1` module. Run unit tests for the module `eunit:test(intro_ex1).`.
- Notice you got error that `add_three` function returns `ok`, as the name suggests it should return a sum of its arguments.

#### Tips

Follow this cycle to code:
 - Make small change in the code.
 - Recompile with `c(intro_ex1).`.
 - Test with `eunit:test(intro_ex1).`.
 - Repeat.

You can shorten recompile and test into a single command: `c(intro_ex1), eunit:test(intro_ex1).`, but if compilation fails you will never know, therefore use `{ok, intro_ex1} = c(intro_ex1), eunit:test(intro_ex1).`

## Ex 2 - Average

Try to follow what you have learned in Ex1 and implement function that takes a list and returns average of its elements. Average is a sum of elements divided by their amount. Start with splitting problem into smaller parts: summing elements of a list, and finding list length. Implement a function `sum/1`, consider two cases: 
 - sum of elements of empty list `[]` is `0`;
 - sum of nonempty list is a sum of head of a list summed with a sum of tail of a list.

```erlang
List = [1, 2, 3, 4],
[Head | Tail] = List,
1 = Head,
[2, 3, 4] = Tail
```

For length you can use the function that already exists, see: http://erlang.org/doc/man/erlang.html#length-1
Congratulations, you've just managed to implement you first function :)

Next steps:
 - Try implementing length function yourself.
 - Try implementing sum function with:
  - tail recursion see: https://stackoverflow.com/questions/33923/what-is-tail-recursion
  - use one of fold functiona see: http://erlang.org/doc/man/lists.html#foldl-3 http://erlang.org/doc/man/lists.html#foldr-3


## Ex 3 - FizzBuzz

Write a function that returns the numbers from 1 to 100. But for multiples of three replaces a number with `fizz` instead of the number and for the multiples of five replaces a number with `buzz`. For numbers which are multiples of both three and five replaces a number with `fizzbuzz`.

Try different implementations:
 - Using function [Pattern Matching](http://erlang.org/doc/reference_manual/patterns.html)
 - Using [`case of .. end`](http://erlang.org/doc/reference_manual/expressions.html#case) construction.

### Tips

Try writing a function to determine output over a single number, and than generate a list `[1, 2, 3, 4, ..., 100]` and map the function over a generated list.

Useful functions:
 - [generating a list sequence](http://erlang.org/doc/man/lists.html#seq-2)
 - [map function](http://erlang.org/doc/man/lists.html#map-2)
 - [alternatively to maps use list comprehension](http://erlang.org/doc/programming_examples/list_comprehensions.html)

## Ex 4 - Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Write a function `thousand_sum/0` to find the sum of all the multiples of 3 or 5 below 1000.

Good job, you've just solved first of: [Project Euler problems](https://projecteuler.net/problem=1).

