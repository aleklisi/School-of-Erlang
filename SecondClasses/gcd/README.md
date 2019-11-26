# Ex 2.1 Greatest common divider

In mathematics, the greatest common divisor (gcd) of two or more integers, which are not all zero, is the largest positive integer that divides each of the integers.
For example, the gcd of 8 and 12 is 4.

Try implementing gcd in Erlang. Use recursion and head matching.
In file `src/gcd.erl` comment `case_gcd(A, B).` and uncomment `head_match_gcd(A, B).`.
Then implement `head_match_gcd/2`.
Use `rebar3 eunit` to test your solution.

## Tips

The algorithm in the other languages. (I hope) you know, at least one of them:

Python:

```python
def gcd(a,b):
	if(b==0): 
		return a
	else: 
		return gcd(b,a % b)
```

Java:

```java
 public static int gcd(int p, int q) {
        if (q == 0) {
            return p;
        }
        return gcd(q, p % q);
    }
```

JavaScript

```javascript
function gcd_two_numbers(x, y) {
  while(y) {
    var t = y;
    y = x % y;
    x = t;
  }
  return x;
}
```
