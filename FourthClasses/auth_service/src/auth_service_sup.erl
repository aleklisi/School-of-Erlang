%%%-------------------------------------------------------------------
%% @doc auth_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(auth_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 2,
                 period => 60},
    ChildSpecs = [#{
        id => auth_service,
        start => {user_exists, start_link, [user_exists_name]}
    }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
