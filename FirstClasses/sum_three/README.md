## Ex 1 - Sum three

- Go to file `FirstClasses/sum_three/src/sum_three.erl` and find `add_three/3` function.
- Change terminal location to `$REPO/FirstClasses/sum_three/`.
- Run unit tests for the exercise `rebar3 eunit`.
- Notice you got error that `add_three` function returns `ok`, as the name suggests it should return a sum of its arguments.
- Try implementing the `add_three/3` function and make all tests pass, as you probably guessed it should return the sum the sum of its arguments.

#### Tips

- `rebar3` is an Erlang package manager. It allows to automatically add and download dependencies, recompile and test your project.
