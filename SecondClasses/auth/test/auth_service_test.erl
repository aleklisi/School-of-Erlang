-module(auth_service_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

% TODO replace module name with your module implementation
-define(IMPLEMENTATION, list_based_auth_service).

-import(?IMPLEMENTATION, [
        add_user/3,
        del_user/2,
        user_exists/2,
        auth_user/3
    ]).

%% -------------------------------------------------------
%% TESTS
%% -------------------------------------------------------
%% -------------------------------------------------------
%% adding a user
%% -------------------------------------------------------

added_user_exists_in_a_state_test() ->
    InitState = init_state(),
    AddedUsersLogin = randomize_name("Adam"),
    AddedUsersPassword = randomize_name("admin"),
    OneUserState = add_user(AddedUsersLogin, AddedUsersPassword, InitState),
    ?assert(user_exists(AddedUsersLogin, OneUserState)),
    ?assertEqual(1, state_size(OneUserState)),
    ok.

many_added_users_exist_in_a_state_test() ->
    InitState = init_state(),
    AddedUsersLogin1 = randomize_name("Adam"),
    AddedUsersPassword1 = randomize_name("admin"),
    AddedUsersLogin2 = randomize_name("Basia"),
    AddedUsersPassword2 = randomize_name("secure"),
    AddedUsersLogin3 = randomize_name("Celina"),
    AddedUsersPassword3 = randomize_name("ugh"),
    OneUserState = add_user(AddedUsersLogin1, AddedUsersPassword1, InitState),
    TwoUsersState = add_user(AddedUsersLogin2, AddedUsersPassword2, OneUserState),
    ThreeUsersState = add_user(AddedUsersLogin3, AddedUsersPassword3, TwoUsersState),
    ?assert(user_exists(AddedUsersLogin1, ThreeUsersState)),
    ?assert(user_exists(AddedUsersLogin2, ThreeUsersState)),
    ?assert(user_exists(AddedUsersLogin3, ThreeUsersState)),
    ?assertEqual(3, state_size(ThreeUsersState)),
    ok.

user_added_twice_exists_once_in_a_state_test() ->
    InitState = init_state(),
    AddedUsersLogin = randomize_name("Adam"),
    AddedUsersPassword = randomize_name("admin"),
    OneUserState = add_user(AddedUsersLogin, AddedUsersPassword, InitState),
    UserAddedTwiceState = add_user(AddedUsersLogin, AddedUsersPassword, OneUserState),
    ?assert(user_exists(AddedUsersLogin, UserAddedTwiceState)),
    ?assertEqual(1, state_size(OneUserState)),
    ok.

not_added_user_does_not_exist_in_a_state_test() ->
    InitState = init_state(),
    AddedUsersLogin = randomize_name("Adam"),
    AddedUsersPassword = randomize_name("admin"),
    NotAddedUserLogin = randomize_name("Basia"),
    OneUserState = add_user(AddedUsersLogin, AddedUsersPassword, InitState),
    ?assertNot(user_exists(NotAddedUserLogin, OneUserState)),
    ok.

%% -------------------------------------------------------
%% deleting a user
%% -------------------------------------------------------

deleted_user_does_not_exist_in_a_state_test() ->
    InitState = init_state(),
    RemovedUsersLogin = randomize_name("Adam"),
    RemovedUsersPassword = randomize_name("admin"),
    OneUserState = add_user(RemovedUsersLogin, RemovedUsersPassword, InitState),
    OneUserRemovedState = del_user(RemovedUsersLogin, OneUserState),
    ?assertNot(user_exists(RemovedUsersLogin, OneUserRemovedState)),
    ?assertEqual(0, state_size(OneUserRemovedState)),
    ok.

added_twice_and_then_deleted_user_does_not_exist_in_a_state_test() ->
    InitState = init_state(),
    RemovedUsersLogin = randomize_name("Adam"),
    RemovedUsersPassword = randomize_name("admin"),
    OneUserState = add_user(RemovedUsersLogin, RemovedUsersPassword, InitState),
    OneUserState1 = add_user(RemovedUsersLogin, RemovedUsersPassword, OneUserState),
    OneUserRemovedState = del_user(RemovedUsersLogin, OneUserState1),
    ?assertNot(user_exists(RemovedUsersLogin, OneUserRemovedState)),
    ?assertEqual(0, state_size(OneUserRemovedState)),
    ok.

deleted_not_existing_user_does_not_affect_state_test() ->
    InitState = init_state(),
    NotRemovedUsersLogin = randomize_name("Adam"),
    NotRemovedUsersPassword = randomize_name("admin"),
    RemovedUsersLogin = randomize_name("Basia"),
    OneUserState = add_user(NotRemovedUsersLogin, NotRemovedUsersPassword, InitState),
    OneUserRemovedState = del_user(RemovedUsersLogin, OneUserState),
    ?assertNot(user_exists(RemovedUsersLogin, OneUserRemovedState)),
    ?assertEqual(1, state_size(OneUserRemovedState)),
    ok.

deleting_one_user_does_not_affect_other_users_test() ->
    InitState = init_state(),
    AddedUsersLogin1 = randomize_name("Adam"),
    AddedUsersPassword1 = randomize_name("admin"),
    AddedUsersLogin2 = randomize_name("Basia"),
    AddedUsersPassword2 = randomize_name("secure"),
    AddedUsersLogin3 = randomize_name("Celina"),
    AddedUsersPassword3 = randomize_name("ugh"),
    OneUserState = add_user(AddedUsersLogin1, AddedUsersPassword1, InitState),
    TwoUsersState = add_user(AddedUsersLogin2, AddedUsersPassword2, OneUserState),
    ThreeUsersState = add_user(AddedUsersLogin3, AddedUsersPassword3, TwoUsersState),
    NewTwoUsersState = del_user(AddedUsersLogin2, ThreeUsersState),
    ?assert(user_exists(AddedUsersLogin1, NewTwoUsersState)),
    ?assertNot(user_exists(AddedUsersLogin2, NewTwoUsersState)),
    ?assert(user_exists(AddedUsersLogin3, NewTwoUsersState)),
    ?assertEqual(2, state_size(NewTwoUsersState)),
    ok.

%% -------------------------------------------------------
%% authenticate a user
%% -------------------------------------------------------

% TODO implement following tests  based on what you see above
% if names are not explanatory enough, please ask :)

user_with_correct_password_is_authenticated_test() ->
    ok.

user_with_incorrect_password_is_not_authenticated_test() ->
    ok.

not_existing_user_is_not_authenticated_test() ->
    ok.

added_and_then_deleted_user_in_not_authenticated_test() ->
    ok.

%% -------------------------------------------------------
%% HELPERS
%% -------------------------------------------------------

state_size(ListState) when is_list(ListState) ->
    length(ListState);
state_size(_State) ->
    % TODO IMPLEMENT FOR MAPS
    % see http://erlang.org/doc/man/maps.html#size-1
    1.

init_state() ->
    init_state(?IMPLEMENTATION).

% TODO Add case for your module
init_state(list_based_auth_service) -> [].

randomize_name(Base) ->
    RandomBinary = base64:encode(crypto:strong_rand_bytes(6)),
    Base ++ binary_to_list(RandomBinary).
