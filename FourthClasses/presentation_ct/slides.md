class: middle, center

# Common Test and Testing in Erlang

School of Erlang

---
# What is common test?

_Common Test_ is a portable application for automated testing and a framework for testing Erlang applications.

It is much more extended than _eunit_ and provides convenient abstractions for tests setup and clean up.

---
# What do I need to remember about?

Each _ct_ test file (module) must be named *_SUITE.erl.

Include the _ct lib_ like this:

```erlang
-module(my_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
```

---
# Init and end per suite

Each test suite module can contain the optional configuration functions _init_per_testcase/2_ and _end_per_testcase/2_.
If the init function is defined, so must the end function be.

If _init_per_testcase_ exists, it is called before each test case in the suite.
It typically contains initialization that must be done for each test case (analog to init_per_suite for the suite).

_end_per_testcase/2_ is called after each test case has finished, enabling cleanup after init_per_testcase.

---
# Test cases

The smallest unit that the test server is concerned with is a test case.
Each test case can test many things,
for example, make several calls to the same interface function with different parameters.

---
# Live demo

Open `FourthClasses/supervion_trees/test/example_SUITE.erl` in the text editor.

Run:
```
cd SchoolOfErlang/FourthClasses/supervion_trees
rebar3 ct --suite=test/example_SUITE
```

and open in a browser: `SchoolOfErlang/FourthClasses/supervion_trees/_build/test/logs/all_runs.html`

---
# gen_server two testing approaches

- starting a gen_server and then operating on it
- testing callbacks without starting a gen_server

---
# Read more 

[ct suites templates](https://erlang.org/doc/apps/common_test/example_chapter.html)

[ct module doc](http://erlang.org/doc/man/ct.html)
