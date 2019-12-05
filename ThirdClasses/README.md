# Ex 3.2 Make you first Erlang application

This exercise will show you how to start an Erlang application form scratch.

The application will have one gen_server which will hold a State with a a list of users and 3 functionalities:

- add a user
- delete a user
- check if users exists and return true or false

## Lets do it

Lets use rebar3 to make new application from the template:

```
rebar3 new app auth_service
cd auth_service
```

Now lets create a file we will be implementing a gen server in:

```
touch src/user_exists.erl 
```

now try `rebar3 compile` it fails:

```
===> Verifying dependencies...
===> Compiling auth_service
===> Compiling src/user_exists.erl failed
src/user_exists.erl:1: no module definition
```

so, lets start with adding `-module(user_exists).` at the beginning in the file you created and recompile.
Now it should work just fine.

Now lets add `-behaviour(gen_server).` as a second line, and recompile again. Wen get 3 warnings:

```
src/user_exists.erl:3: Warning: undefined callback function handle_call/3 (behaviour 'gen_server')
src/user_exists.erl:3: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
src/user_exists.erl:3: Warning: undefined callback function init/1 (behaviour 'gen_server')
```

try addressing them by implementing the required function.

### Implementing gen_server

#### init

See the [docs](https://erlang.org/doc/man/gen_server.html#Module:init-1).

Lets use a list as a state to store users logins.
Lets notice that at the beginning we have no users in the system so the list of users should be `[]` empty.

#### User adding

Start with adding a function `add_user` which calls:

```erlang
add_user(Login) ->
   gen_server:call(?MODULE, {add_user, Login}).
```

Now handle the call in the handle call function:

```
handle_call({add_user, Login}, _From, State) ->
   NewState = [Login | State],
    {reply, added, NewState}.
```

Implement the following two callbacks the same way.

### Tips

Erlang plugin for VSCode has a template for gen_server, press Cmd + P and than type `>Insert Snippet` and pick gen_server.
But in case you editor does not have it you can use this template:
```erlang
-module(user_exists).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    State = ok,
   {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
```


Checkout [this module](https://github.com/aleklisi/School-of-Erlang/blob/master/SecondClasses/auth/src/list_based_auth_service.erl) from previous classes.
Notice that if you remove passwords part from it you will get state handling for you.

### Plug the gen_server into the supervision tree

got to `ThirdClasses/auth_service/src/auth_service_sup.erl` file and replace:

```erlang
ChildSpecs = [],
```

with:

```erlang
ChildSpecs = [#{id => user_exists,
                    start => {user_exists, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [user_exists]}],
```
For now assume that it just works.
We will discuss supervisor details on the next classes.

now lets try running the app:

```
rebar3 shell

user_exists:add_user("Alek").
added
user_exists:add_user("Nelson").
added
```

Well done!

### Code hot swapping

This section will show you AWESOME feature of Erlang which is code hot swapping on running processes:

so lets start the app:

```
rebar3 shell
user_exists:add_user("Alek").
```

Lets notice that `added` is returned.

Now go to the text editor to the file `ThirdClasses/auth_service/src/user_exists.erl`.
Replace `handle_call` return value from:

```
{reply, added, NewState}.
```

with

```
{reply, {added, Login}, NewState}.
```


next recompile the module in the erlang console, and add one more user:

```
c(user_exists).
user_exists:add_user("Nelson").
```

Lets notice that `{added,"Nelson"}` is returned and gen_server is still alive.

If you like it checkout [This article](https://github.com/nickmcdowall/Erlang-Examples/wiki/Hot-Code-Swapping).

### Observer

A GUI tool for observing an Erlang system.

```
rebar3 shell

observer:start().
```

A new window opens and you can use it


[docs](A GUI tool for observing an Erlang system.)
