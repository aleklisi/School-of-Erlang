# Ex 4.2 Finishing auth service


## Crushing the application

Lets see what happens when we try to crush the gen_server.

The default service name is `user_exists_name`.
```
rebar3 shell

user_exists:crush(user_exists_name).
user_exists:crush(user_exists_name).
user_exists:crush(user_exists_name).
user_exists:crush(user_exists_name).
```

Lets notice that for first 3 times we get not only ok but also a long crush report explaining in detail what went wrong.
But after that we only get ok which is always a result of *gen_server:cast/2*.

## Finishing the implementations

Finish the `user_exists` module implementation using maps.
Implement:

 - delete_user/2,
 - exists_user/2,
 - authenticate_user/3

Which  of the functions can be done as `casts` and not `calls`?

Be careful, you may get a race condition when adding and updating data with casting.

Implement the remaining test by porting testacies from `test/auth_service_test.erl` which you were implementing on the second classes.
For your convenience I included it in this project.

## Distributed Erlang

Lets try running Erlang in a distributed form.

We need 2 terminal windows, for my convenience, I will be naming them _left_ and _right_ one.

Left terminal:

```
cd $PATH_TO_REPO/SchoolOfErlang/FourthClasses/auth_service
rebar3 shell --name auth@127.0.0.1 --setcookie my_cookie

node().
nodes().
```

Right terminal:

```
erl -name client@127.0.0.1 -setcookie my_cookie

node().
nodes().

net_adm:ping('auth@127.0.0.1').

node().
nodes().

rpc:call('auth@127.0.0.1', user_exists, authenticate_user, [user_exists_name, "Alek", "Pass"]).
```

should return `false`.

Left terminal:

Lets now add a user:

```
user_exists:add_user(user_exists_name, "Alek", "Pass").
```

Right terminal:

And try again:

```
rpc:call('auth@127.0.0.1', user_exists, authenticate_user, [user_exists_name, "Alek", "Pass"]).
```

should return `true`.

WELL DONE!! 

You just executed some code on a remote node.

[Distributed Erlang docs](https://erlang.org/doc/reference_manual/distributed.html)

