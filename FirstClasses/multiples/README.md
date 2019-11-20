## Ex 4 - Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Write a function `thousand_sum/0` to find the sum of all the multiples of 3 or 5 below 1000.

Good job, you've just solved first of: [Project Euler problems](https://projecteuler.net/problem=1).

### Tips

- Start with writing a function to generate list [1, 2, 3, ..., 1000]. 
- Next, write a function to filter the elements that are not multiplies of 3 or 5. [filter function](http://erlang.org/doc/man/lists.html#filter-2) might be useful.
- Last sum elements of a list. You should already have a function for that (see Ex 2).
