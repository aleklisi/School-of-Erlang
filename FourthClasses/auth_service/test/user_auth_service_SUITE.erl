-module(user_auth_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    RandomBytes = base64:encode(crypto:strong_rand_bytes(10)),
    AuthServiceName = binary_to_atom(RandomBytes, utf8),
    user_exists:start_link(AuthServiceName),
    Login = "ExistingUser",
    Password = "ExistingUsersPassword",
    user_exists:add_user(AuthServiceName, Login, Password),
    [{existing_user_creds, {Login, Password}}, {auth_service_name, AuthServiceName} | Config].

end_per_testcase(_Case, Config) ->
    AuthServiceName = proplists:get_value(auth_service_name, Config),
    user_exists:stop(AuthServiceName),
    ok.

groups() ->
    [
        {user_login_and_password_validation, [parallel], [
            when_user_is_added_he_or_she_is_authenticated_correctly,
            when_user_is_not_added_he_or_she_fails_authentication
        ]},
        {user_exists, [], [
            added_user_exists_in_a_state_test,
            not_added_user_exists_in_a_state_test
        ]}
    ].

all() ->
   [
        {group, user_login_and_password_validation},
        {group, user_exists}
   ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

when_user_is_added_he_or_she_is_authenticated_correctly(Config) ->
    AuthServiceName = proplists:get_value(auth_service_name, Config),
    {Login, Password} = proplists:get_value(existing_user_creds, Config),
    user_exists:add_user(AuthServiceName, Login, Password),
    true = user_exists:authenticate_user(AuthServiceName, Login, Password),
    ok.

when_user_is_not_added_he_or_she_fails_authentication(Config) ->
    AuthServiceName = proplists:get_value(auth_service_name, Config),
    false = user_exists:authenticate_user(AuthServiceName, "NotExisting", "NotExistingPassword"),
    ok.

added_user_exists_in_a_state_test(Config) ->
    AuthServiceName = proplists:get_value(auth_service_name, Config),
    {Login, _} = proplists:get_value(existing_user_creds, Config),
    true = user_exists:exists_user(AuthServiceName, Login),
    ok.

not_added_user_exists_in_a_state_test(Config) ->
    AuthServiceName = proplists:get_value(auth_service_name, Config),
    false = user_exists:exists_user(AuthServiceName, "NotExisting"),
    ok.

