-module(crowl).

% {ok, _} = c(crowl), crowl:main().
-define(URL_BASE, "https://en.wikipedia.org").

-define(START, "https://en.wikipedia.org/wiki/Haskell_(programming_language)").
-define(END, "https://en.wikipedia.org/wiki/Adolf_Hitler").

-compile(export_all).

main() ->
    start_env(),
    ToVisit = [?START],
    CoordinatorPid = spawn_link(fun() ->
        coordinator(digraph:new(), sets:new(), queue:from_list(ToVisit))
    end),
    [spawn(fun() ->
        worker(CoordinatorPid)
    end) || _ <- lists:seq(1, 10)].


start_env() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets).

coordinator(Graph, Visited, Queue) ->
    {NewGraph, NewVisited, NewToVisit} =
    receive
        M = {waiting, Pid} ->
            case queue:is_empty(Queue) of
                true ->
                    erlang:send_after(100, self(), M),
                    {Graph, Visited, Queue};
                false -> 
                    NextUrl = queue:get(Queue),
                    Tail = queue:drop(Queue),
                    Pid ! {new_task, NextUrl},
                    {Graph, Visited, Tail}
            end;
        {crowl_result, Url, NewUrls} ->
            FilteredNew = lists:filter(
                fun(NewUrl) ->
                    not sets:is_element(NewUrl, Visited)
            end, NewUrls),
            ExtendedVisited = sets:add_element(Url, Visited),
            ParentVertex = digraph:add_vertex(Graph, Url, Url),
            lists:map(fun(NewVertex) ->
                ChildVertex = digraph:add_vertex(Graph, NewVertex, NewVertex),
                digraph:add_edge(Graph, ParentVertex, ChildVertex)
            end, FilteredNew),
            spawn(fun() -> maybe_get_path(Graph, ExtendedVisited) end),
            {Graph, ExtendedVisited, queue:join(Queue, queue:from_list(FilteredNew))};
        Msg ->
            io:fwrite("Unexpected message = ~p\n", [Msg]),
            {Graph, Visited, Queue}
    end,
    coordinator(NewGraph, NewVisited, NewToVisit).

maybe_get_path(Graph, Visited) ->
    case sets:is_element(?END, Visited) of
        false ->
            io:fwrite("~p\n", [digraph:no_vertices(Graph)]),
            io:fwrite("No path yet\n");
        Path ->
            Path = digraph:get_path(Graph, ?START, ?END),
            io:fwrite("~p\n", [digraph:no_vertices(Graph)]),
            io:fwrite("Path is ~p\n", [Path])
            % erlang:error(done)
        end.

worker(CoordinatorsPid) ->
    CoordinatorsPid ! {waiting, self()},
    receive
        {new_task, Url} ->
            Body = crowl(Url),
            NewUrls = get_links(Body),
            CoordinatorsPid ! {crowl_result, Url, NewUrls};
        Msg ->
            io:fwrite("Unexpected message = ~p\n", [Msg])
    end,
    worker(CoordinatorsPid).

crowl(Url) ->
    case httpc:request(Url) of
        {ok, {{_, 200, "OK"}, _Headers, Body}} ->
            Body;
        % Error handling 404 and others
        _ -> ""
    end.

get_links(Website) ->
    {ok, MP} = re:compile("<a href=\"(/wiki/[\\w|(|)]+)", [ucp]),
     case re:run(Website, MP, [global, {capture, all, list}]) of
         {match, Matches} ->
            sets:to_list(sets:from_list(
                lists:map(
                    fun([_, Link]) ->
                        ?URL_BASE ++ Link
                    end, Matches)));
        nomatch -> []
    end.
