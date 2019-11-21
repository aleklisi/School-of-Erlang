# Modules and Functions

An *application* consists of *modules* and each module consists of either *module attributes*, *functions* and *comments*.

## Comments

`%` is used to comment single line. There are no multi-line comments in Erlang.

```erlang
% Notice that `boo` is returned instead of `bar` it is commented.
foo() -> % bar.
    boo.
```

## Module Attributes

### `-module(Module).`

*Module* declaration, defining the name of the module.
The name `Module`, an atom, is to be same as the file name minus the extension _.erl_.
Otherwise code loading does not work as intended. 

Eg. file `cat.erl` must contain:

```erlang
-module(cat).
```

This attribute is to be specified first and is the only mandatory attribute.

### `-export(Functions).`

Exported functions. Specifies which of the functions, defined within the module, that are visible from outside the module.

Functions is a list `[Name1/Arity1, ..., NameN/ArityN]`, where each `NameI` is an _atom_ and `ArityI` an _integer_.

Exported functions are public (accessible from everywhere) and *all* other functions are private (accessible from inside module only).

```erlang
-module(cat).
-export([meow/0]).

meow() ->
    meow.

eat(Food) ->
    {eaten, Food}.

c(cat).
cat.erl:7: Warning: function eat/1 is unused
{ok,cat}
2> cat:meow().
meow
3> cat:eat(mouse).
** exception error: undefined function cat:eat/1
```

Notice the warning for not exported function during compilation.

[See more](http://erlang.org/doc/reference_manual/modules.html)


## Functions

A *function declaration* is a sequence of *function clauses* separated by _semicolons_, and terminated by _period_ `.`.

A *function clause* consists of a clause head and a clause body, separated by `->`.

A clause head consists of the *function name*, an *argument list*, and an optional *guard* sequence beginning with the keyword when:

```erlang
Name(Pattern11,...,Pattern1N) [when GuardSeq1] ->
    Body1;
...;
Name(PatternK1,...,PatternKN) [when GuardSeqK] ->
    BodyK.
```

The function name is an `atom`. Each argument is a pattern.

There are only a few [functions allowed](http://erlang.org/doc/reference_manual/expressions.html#guard-sequences) in the *guards*.

Each `BodyN` cosmists of *Expressions* separated by `,`.

A function always returns its last expression (the one before the dot).

### Expressions

#### Function calls

```erlang
ExprFunction(Expr1,...,ExprN)
ExprModule:ExprFunction(Expr1,...,ExprN)

% The result can be also assigned to a variable like this:

foo(List) ->
    SortedList = lists:sort(List), % A list List is sorted and assigned to SortedList variable
    SortedWithoutFirst = lists:droplast(SortedList), % Last element is deleted but as data is immutable old list is not changed.
    {SortedList, SortedWithoutFirst}. % 

foo([1,3,2,4]).
{[1, 2, 3, 4], [2, 3, 4]}
```

##### case

The expression `Expr` is evaluated and the patterns `Pattern` are sequentially matched against the _result_. If a match succeeds and the optional guard sequence `GuardSeq` is true, the corresponding `Body` is evaluated.
The return value of `Body` is the return value of the case expression.
If there is no matching pattern with a true guard sequence, a case_clause run-time error occurs.

is_even(MaybeNumber) ->
    case MaybeNumber of
         N when is_integer(N) andalso 5 rem 2 == 0 ->
            {is_even, N};
        N when is_integer(N) andalso 5 rem 2 == 1 -> % andalso is logical and with short-circuiting so it will not fail if N is not an integer
            {is_odd, N};
        Else ->
            {is_not_a_number, Else}
    end.

#### Lists

The list concatenation operator `++` appends its second argument to its first and returns the resulting list.
The list subtraction operator `--` produces a list that is a copy of the first argument.
The procedure is a follows: for each element in the second argument, the first occurrence of this element (if any) is removed.

```erlang
1> [1,2,3]++[4,5].
[1,2,3,4,5]
2> [1,2,3,2,1,2]--[2,1,2].
[3,1,2]
```

## Variables

A variable is an expression. If a variable is bound to a value, the return value is this value. Unbound variables are only allowed in patterns. Variables start with an uppercase letter or underscore (`_`). Variables can contain alphanumeric characters, underscore and `@`.
Variables start with capital letters like `List`, `Result`,...

### Immutability and matching

Variables are matched and once assigned they are immutable:

```erlang
1> M = 5.
5
2> M = 6.
** exception error: no match of right hand side value 6
3> M = M + 1.
** exception error: no match of right hand side value 6
4> N = M + 1.
6
```

You can also match on parts of more complex structures:

```erlang
1> {Response, Result} = {aborded, {error, already_defined}}.
{aborded,{error,already_defined}}
2> Response.
aborded
3> Result.
{error,already_defined}
```

### Shadowing

Lets consider following example. We want to do some action for first element of the list and do something else for the tail of the list.

```erlang
-module(m).
-export([foo/1]).

foo(List = [ H | _T ]) ->
    io:fwrite("Outer H = ~p\n", [H]),
    lists:map(
        fun(H) ->
            io:fwrite("Inner H = ~p\n", [H]);
           (NotH) ->
            io:fwrite("Inner NotH = ~p\n", [NotH])
        end, List).

1> c(m).
m.erl:8: Warning: variable 'H' shadowed in 'fun'
m.erl:10: Warning: this clause cannot match because a previous clause at line 8 always matches
{ok,m}
2> m:foo([1, 2, 3, 4]).
Outer H = 1
Inner H = 1
Inner H = 2
Inner H = 3
Inner H = 4
[ok,ok,ok,ok]
```
First, lets notice the to warnings we have during compilation. All calls went to match the function clause.
`H` variable was shadowed. To fix it change code to the following:

```erlang
-module(m).
-export([foo/1]).

foo(List = [ Head | _T ]) ->
    io:fwrite("Outer H = ~p\n", [Head]),
    lists:map(
        fun(H) when H == Head -> % Notice how the change
            io:fwrite("Inner H = ~p\n", [H]);
           (NotH) ->
            io:fwrite("Inner NotH = ~p\n", [NotH])
        end, List).

17> c(m).
{ok,m}
18> m:foo([1, 2, 3, 4]).
Outer H = 1
Inner H = 1
Inner NotH = 2
Inner NotH = 3
Inner NotH = 4
[ok,ok,ok,ok]
```

See more about:
 - [functions](http://erlang.org/doc/reference_manual/functions.html)
 - [sequential programing](http://erlang.org/doc/getting_started/seq_prog.html)
 - [expressions syntax](http://erlang.org/doc/reference_manual/expressions.html)
