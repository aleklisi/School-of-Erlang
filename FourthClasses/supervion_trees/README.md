# Ex 4.1 Supervision trees

Try building following supervision trees:

## 4.1.1

```
           top_sup
        /     |    \ 
    a_sup  b_sup   c_sup
```

To test you solution run:

`rebar3 ct --suite=test/horisontal_sup_tree_SUITE`

## 4.1.2

```
           top_sup
              |
            a_sup
              |
            b_sup
              |
            c_sup
```
To test you solution run:

`rebar3 ct --suite=test/vertical_tree_sup_SUITE`

## 4.1.3

```
           top_sup
          /       \ 
        a_sup    c_sup
          |
        b_sup
```

To test you solution run:

`rebar3 ct --suite=test/mixed_sup_tree_SUITE`

## Tips

### Observer
Use observer to see your application structure. To start an observer:

```
rebar3 shell
observer:start()
```

### ct vs eunit

We also switched from using _eunit_ to using _ct_ (common test),
therefore to run the test you have to use command `rebar3 ct` instead of `rebar3 eunit`.

See the tests report in a browser:

`$PATH_TO_REPOfile://$PATH_TO_REPO/SchoolOfErlang/FourthClasses/supervion_trees/_build/test/logs/all_runs.html`