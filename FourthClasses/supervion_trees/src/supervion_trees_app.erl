-module(supervion_trees_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    top_sup:start_link().

stop(_State) ->
    ok.
