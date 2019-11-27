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
Use `lists:filter/2`.

## Task 1.2 - Quicksort

Refactor your function to filter using list comprehension.

## Task 2 - Improve filtering

Implement you own filtering function which will divide the list into to subsets on one iteration.

## Task 3 - Ordered property

Try making `quicksort_test:ordered/1` tail recursive.

## Task 4 - performance competition

Implement a test compering performance solution.
