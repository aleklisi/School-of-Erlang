-module(list_based_auth_service).

-author('aleksander.lisiecki@erlang-solutions.com').

-behavior(auth_service_behavior).

-type login() :: string().
-type password() :: string().
-type state() :: {login(), password()}.

-export([
        add_user/3,
        del_user/2,
        user_exists/2,
        auth_user/3
    ]).

-spec add_user(login(), password(), state()) -> state().
add_user(Login, Password, State) ->
    State0 = case user_exists(Login, State) of
        true -> del_user(Login, State);
        false -> State
    end,
    [{Login, Password} | State0].

-spec del_user(login(), state()) -> state().
del_user(Login, State) ->
    lists:filter(
        fun({UserLogin, _}) when Login == UserLogin -> false;
           (_) -> true
        end, State).

-spec user_exists(login(), state()) -> boolean().
user_exists(Login, State) ->
    lists:any(
        fun({UserLogin, _}) when Login == UserLogin -> true;
           (_) -> false
        end, State).

-spec auth_user(login(), password(), state()) -> boolean().
auth_user(Login, Password, State) ->
    lists:any(
        fun({UserLogin, UserPassword}) ->
            Login == UserLogin andalso Password == UserPassword
        end, State).
