# Introduction to Erlang

## Name origin

- Danish mathematician and engineer Agner Krarup *Erlang*;
- A syllabic abbreviation of *Er*icsson *Lang*uage.

## Open Telecom Platform

OTP is a collection of useful middleware, libraries, and tools written in the Erlang programming language. This is the actual power o Erlang.

## Data types

See http://erlang.org/doc/reference_manual/data_types.html for reference.

See http://erlang.org/doc/efficiency_guide/advanced.html for reference.
### Term

A piece of data of any data type is called a term.

### Numbers

#### Integers

Large integers are automatically supported.

Examples:

```elrang
1> 5.
5
2> -10.
-10
3> - 10.
-10
4> 16#f2.
242
```

#### Floats

Examples:

```erlang
1> 3.14.
3.14
2> 3.14e2.
314.0
3> 3.14e-2.
0.0314
```

### Atoms

