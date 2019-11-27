-module(auth_service_behavior).

-type login() :: string().
-type password() :: string().
-type state() :: #{login() => password()} | [{login(), password()}].

-callback add_user(login(), password(), state()) -> state().

-callback del_user(login(), state()) -> state().

-callback user_exists(login(), state()) -> boolean().

-callback auth_user(login(), password(), state()) -> boolean().
