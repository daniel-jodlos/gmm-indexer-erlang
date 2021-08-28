%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that receives events
%%%  and potentially redirects them to proper outbox
%%% @end
%%%-------------------------------------------------------------------
-module(inbox).
-author("pawel").

%% API
-export([
    start_link/1,
    init_inbox/1,
    post/2,
    is_empty/0,
    free_vertex/1
]).


%%%---------------------------
%% Exported functions
%%%---------------------------

start_link(Vertex) ->
    case ets:member(inboxes, Vertex) of
        true -> ets:update_element(inboxes, Vertex, {2, null});
        false -> ets:insert(inboxes, {Vertex, null, true, []})
    end,
    {ok, spawn_link(inbox, init_inbox, [Vertex])}.

init_inbox(Vertex) ->
    ets:update_element(inboxes, Vertex, {2, self()}),
    servant(Vertex).

-spec post(Vertex :: binary(), Event :: map()) -> ok.
post(Vertex, Event) ->
    ThisZone = gmm_utils:zone_id(),
    case gmm_utils:owner_of(Vertex) of
        ThisZone -> local_post(Vertex, Event);
        _Other -> outbox:post(Vertex, Event)
    end.

-spec is_empty() -> boolean().
is_empty() ->
    Count = ets:foldl(
        fun({_, Pid, _, _}, Acc) ->
            Pid ! {is_empty, self()}, Acc + 1
        end, 0, inboxes),
    collect_answers(true, Count).

-spec free_vertex(Vertex :: binary()) -> free_vertex.
free_vertex(Vertex) ->
    Pid = ets:lookup_element(inboxes, Vertex, 2),
    Pid ! free_vertex.


%%%---------------------------
%% Internal functions
%%%---------------------------

-spec local_post(Vertex :: binary(), Event :: map()) -> ok.
local_post(Vertex, Event) ->
    case ets:member(inboxes, Vertex) of
        true ->
            Pid = ets:lookup_element(inboxes, Vertex, 2),
            Pid ! {event, Event, self()},
            receive
                ok -> ok;
                _ -> {error, unknown_answer}
            after 2000 -> {error, timeout}
            end;
        false ->
            gmm_sup:create_inbox_servant(Vertex),
            local_post(Vertex, Event)
    end.

-spec is_empty(Vertex :: binary()) -> boolean().
is_empty(Vertex) ->
    case ets:lookup_element(inboxes, Vertex, 4) of
        [] -> ets:lookup_element(inboxes, Vertex, 3);
        _ -> false
    end.

-spec collect_answers(Acc :: boolean(), MsgToRead :: integer()) -> boolean().
collect_answers(Acc, 0) -> Acc;
collect_answers(Acc, MsgToRead) ->
    receive
        {is_empty, Bool} -> collect_answers(Acc and Bool, MsgToRead - 1)
    after 1000 -> false
    end.

-spec is_vertex_free(Vertex :: binary()) -> boolean().
is_vertex_free(Vertex) ->
    ets:lookup_element(inboxes, Vertex, 3).

-spec mark_vertex_free(Vertex :: binary()) -> boolean().
mark_vertex_free(Vertex) ->
    ets:update_element(inboxes, Vertex, {3, true}).

-spec mark_vertex_busy(Vertex :: binary()) -> boolean().
mark_vertex_busy(Vertex) ->
    ets:update_element(inboxes, Vertex, {3, false}).

-spec queue_event(Vertex :: binary(), Event :: map()) -> boolean().
queue_event(Vertex, Event) ->
    OldQueue = ets:lookup_element(inboxes, Vertex, 4),
    ets:update_element(inboxes, Vertex, {4, OldQueue ++ [Event]}).

-spec poll_event(Vertex :: binary()) -> none | map().
poll_event(Vertex) ->
    case ets:lookup_element(inboxes, Vertex, 4) of
        [] -> none;
        [Event | Rest] ->
            ets:update_element(inboxes, Vertex, {4, Rest}),
            Event
    end.

%% Servant process

servant(Vertex) ->
    receive
        {is_empty, Pid} ->
            Pid ! {is_empty, is_empty(Vertex)};
        free_vertex ->
            case poll_event(Vertex) of
                none ->
                    mark_vertex_free(Vertex);
                Event ->
                    %% @todo event_processor:process(Event)
                    io:format("Vertex: ~p; Event: ~p\n", [Vertex, Event])
            end;
        {event, Event, Pid} ->
            case is_vertex_free(Vertex) of
                true ->
                    mark_vertex_busy(Vertex),
                    %% @todo event_processor:process(Event)
                    io:format("Vertex: ~p; Event: ~p\n", [Vertex, Event]);
                false ->
                    queue_event(Vertex, Event)
            end,
            Pid ! ok
    end,
    servant(Vertex).
