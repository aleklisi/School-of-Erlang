-module(user_exists).

-behaviour(gen_server).

%% API
-export([
   stop/1,
   start_link/1,
   add_user/3,
   delete_user/2,
   exists_user/2,
   authenticate_user/3,
   crush/1
]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

stop(Name) ->
   gen_server:call(Name, stop).

start_link(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [], []).

add_user(StorageServerName, Login, Password) ->
    gen_server:call(StorageServerName, {add, Login, Password}).

authenticate_user(StorageServerName, Login, Password) ->
    gen_server:call(StorageServerName, {authenticate, Login, Password}).

delete_user(StorageServerName, Login) ->
   ok.

exists_user(StorageServerName, Login) ->
   ok.

crush(StorageServerName) ->
    gen_server:cast(StorageServerName, crush).

%% Callbacks

init(_Args) ->
   {ok, #{}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({add, Login, Password}, _From, State) ->
    {reply, added, State#{Login => Password}};

handle_call({authenticate, Login, Password}, _From, State) ->
    Result = maps:is_key(Login, State) andalso Password == maps:get(Login, State),
    {reply, Result, State}.

handle_cast(crush, State) ->
   1 = 2,
   {noreply, State};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
