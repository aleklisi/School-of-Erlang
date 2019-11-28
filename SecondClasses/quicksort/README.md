# Quicksort

## Algorithm idea

0) If list is empty (or 1 element) list it is sorted.
1) Pick the first element of a list `F`.
2) Filer smaller elements from the the first element `F` of a remaining list to a list `S`.
3) Filer greater or equal elements from the the first element `F` of a remaining list filter to a list `B`.
4) Sort `S` to `SortedS`.
5) Sort `B` to `SortedB`.
6) return `SortedS ++ [] ++ SortedB`.

## Task 1.1 - Quicksort

Implement `quicksort:sort/1` which should sort a given list.

### Tips

Use `lists:filter/2`. to gt smaller and bigger subset.

When you have a number and want to add it to the end of a list you can wrap it to a list like this: 

```erlang
1> List = [1, 2, 3].
[1,2,3]
2> NewLast = 4.
4
3> List ++ [NewLast].
[1,2,3,4]
```

## Task 1.2 - Quicksort

Refactor your function to filter using list comprehension.

## Task 2 - Improve filtering

Implement you own filtering function which will divide the list into to subsets on one iteration.

### Tips

Use fold or tail recursion style where you accumulator is a two elements tuple `{Smaller, Bigger}` and you can start with `{[], []}`.

## Task 3 - Ordered property

Try making `quicksort_test:ordered/1` tail recursive.

## Task 4 - performance competition

Implement a test compering performance solution.
Consider using [timer:tc/1](http://erlang.org/doc/man/timer.html#tc-1) function.