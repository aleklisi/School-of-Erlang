# Data Types

## Number

There are two types of numeric literals, integers and floats.

### Integer

Besides the conventional notation, there are two Erlang-specific notations:
`$char`
ASCII value or unicode code-point of the character char.
`base#value`
Integer with the base base, that must be an integer in the range 2..36.

Erlang integers do not overflow. [see](http://erlang.org/doc/efficiency_guide/advanced.html)
```erlang
1> 42.
42
2> $A.
65
3> $\n.
10
4> 2#101.
5
5> 16#1f.
31
6> 576460752303423488 * 576460752303423488.
332306998946228968225951765070086144
7>
```

### Float

Notice the precision loss in the last case.

```erlang
1> 2.3.
2.3
2> 2.3e3.
2.3e3
3> 2.3e-3.
0.0023
4> 0.1 - 0.2 + 0.3.
0.19999999999999998
``` 

## Atom

An atom is a literal, a constant with name. An atom is to be enclosed in single quotes `'` if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore `_`, or `@`.

```erlang
hello
phone_number
'Monday'
'phone number'
'kitchen@192.168.0.1'
```

## Bit String and Binary

A bit string is used to store an area of untyped memory.
Bit strings are expressed using the [bit syntax](http://erlang.org/doc/reference_manual/expressions.html#bit_syntax). 
Bit strings that consist of a number of bits that are evenly divisible by eight, are called binaries.

```erlang
1> <<10,20>>.
<<10,20>>
2> <<"ABC">>.
<<"ABC">>
3> <<1:1,0:1>>.
<<2:2>>
4> <<"Alek">>.
<<"Alek">>
```

## String

Strings are enclosed in double quotes `"`, but is not a data type in Erlang. Instead, a string `"hello"` is shorthand for the list `[$h,$e,$l,$l,$o]`, that is, `[104,101,108,108,111]`.

Two adjacent string literals are concatenated into one. This is done in the compilation.

```erlang
1> "Al" "ek".
"Alek"
```
So all in all a string is actually a list of chars.

## Reference

A reference is a term that is unique in an Erlang runtime system, created by calling `make_ref/0`.

## Fun

A fun is a functional object. Funs make it possible to create an anonymous function and pass the function itself *not its name* as argument to other functions.

```erlang
1> F = fun(A, B) -> A + B end.
#Fun<erl_eval.13.126501267>
2> F(2, 3).
5
```

When you have a module and you want to pass already defined function to another function use:

```erlang
-module(m).
-export([double_all/1, sin_all/1]).

% within the same module
double_all(List) ->
    lists:map(
        fun double/1,
        List).

double(X) ->
    X * 2.

% or from another module
sin_all(List) ->
    lists:map(
        fun math:sin/1,
        List).

```



## Port Identifier

A port identifier identifies an Erlang port.

`open_port/2`, which is used to create ports, returns a value of this data type.

## Pid

A process identifier, pid, identifies a process.

The following BIFs, which are used to create processes, return values of this data type:

 - `spawn/1,2,3,4`
 - `spawn_link/1,2,3,4`
 - `spawn_opt/4`

```erlang
1> T = fun() -> io:fwrite("Hello") end.
#Fun<erl_eval.21.126501267>
2> spawn(T).
Hello<0.115.0>
```
## Tuple

A tuple is a compound data type with a fixed number of terms:

`{Term1,...,TermN}`

Each term Term in the tuple is called an element. The number of elements is said to be the size of the tuple.

```erlang
1> P = {adam,24,{july,29}}.
{adam,24,{july,29}}
2> element(1,P).
adam
3> element(3,P).
{july,29}
4> P2 = setelement(2,P,25).
{adam,25,{july,29}}
5> tuple_size(P).
3
6> tuple_size({}).
0
```

## Map

A map is a compound data type with a variable number of key-value associations:

`#{Key1=>Value1,...,KeyN=>ValueN}`
Each key-value association in the map is called an association pair. The key and value parts of the pair are called elements. The number of association pairs is said to be the size of the map.

```erlang
1> M1 = #{name=>adam,age=>24,date=>{july,29}}.
#{age => 24,date => {july,29},name => adam}
2> maps:get(name,M1).
adam
3> maps:get(date,M1).
{july,29}
4> M2 = maps:update(age,25,M1).
#{age => 25,date => {july,29},name => adam}
5> map_size(M).
3
6> map_size(#{}).
0
```

## List

A list is a compound data type with a variable number of terms.

`[Term1,...,TermN]`
Each term Term in the list is called an element. The number of elements is said to be the length of the list.

Formally, a list is either the empty list `[]` or consists of a head (first element) and a tail (remainder of the list). The tail is also a list. The latter can be expressed as `[H|T]`. The notation `[Term1,...,TermN]` above is equivalent with the list `[Term1|[...|[TermN|[]]]]`.

Example:

`[]` is a list, thus
`[c|[]]` is a list, thus
`[b|[c|[]]]` is a list, thus
`[a|[b|[c|[]]]]` is a list, or in short `[a,b,c]`

A list where the tail is a list is sometimes called a proper list. It is allowed to have a list where the tail is not a list, for example, `[a|b]`. However, this type of list is of *little* practical use.

```erlang
1> L1 = [a,2,{c,4}].
[a,2,{c,4}]
2> [H|T] = L1.
[a,2,{c,4}]
3> H.
a
4> T.
[2,{c,4}]
5> L2 = [d|T].
[d,2,{c,4}]
6> length(L1).
3
7> length([]).
0
```

## Record

A record is a data structure for storing a fixed number of elements. It has named fields and is similar to a struct in C. However, a record is not a true data type. Instead, record expressions are translated to tuple expressions during compilation. Therefore, record expressions are not understood by the shell unless special actions are taken.

```erlang
-module(person).
-export([new/2]).

-record(person, {name, age}).

new(Name, Age) ->
    #person{name=Name, age=Age}.

1> person:new(ernie, 44).
{person,ernie,44}
```

## Boolean

There is no Boolean data type in Erlang. Instead the atoms true and false are used.
```erlang
1> 2 =< 3.
true
2> true or false.
true
```
See more about:
 - [data types](http://erlang.org/doc/reference_manual/data_types.html)
 - [memory impact & other limitations](http://erlang.org/doc/efficiency_guide/advanced.html)
