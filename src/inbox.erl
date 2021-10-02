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
    start_link/0,
    create_ets_tables/0,
    init_dispatcher/0,
    post/2,
    post/3,
    is_empty/0,
    free_vertex/1
]).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

start_link() ->
    {ok, spawn_link(inbox, init_dispatcher, [])}.

init_dispatcher() ->
    ets:update_element(i_dispatcher, pid, {2, self()}),
    look_for_eligible_queues(),
    dispatcher_routine().

create_ets_tables() ->
    %% i_events; record looks like: {
    %%   {VertexId, Idx} <- {binary(), integer()}, key;
    %%   Event <- event()
    %% }
    ets:new(i_events, [
        named_table, public,
        {write_concurrency, true},
        {decentralized_counters, true}
%%        {read_concurrency, true} % @todo benchmark if it's useful to enable this option
    ]),

    %% i_state_of_queues; record looks like: {
    %%   VertexId <- binary(), key;
    %%   IdxOfLastEvent <- integer(), counter;
    %%   IdxOfFirstEvent <- integer(), counter;
    %%   Free <- boolean(), false if processor handles it's event
    %% }
    ets:new(i_state_of_queues, [named_table, public, {write_concurrency, true}]),

    %% i_dispatcher -- mini table, only 2 distinct objects
    %%  1) {pid, Pid} <- stores pid() of dispatcher process
    %%  2) {ready_queues, Set} <- stores set of vertices which events can be processed
    ets:new(i_dispatcher, [named_table, public]),
    ets:insert(i_dispatcher, {pid, undefined}),
    ets:insert(i_dispatcher, {ready_queues, sets:new()}).

-spec post(Vertex :: binary(), Event :: event()) -> ok.
post(Vertex, Event) ->
    do_post(Vertex, Event, notification:queue(Vertex, Event)).

%% Timestamp is supposed to be value returned by erlang:system_time(nanosecond)
-spec post(Vertex :: binary(), Event :: event(), Timestamp :: integer()) -> ok.
post(Vertex, Event, Timestamp) ->
    BaseNotification = notification:queue(Vertex, Event),
    do_post(Vertex, Event,
        maps:update(time, gmm_utils:nanosecond_timestamp_to_iso6801(Timestamp), BaseNotification)).

-spec is_empty() -> boolean().
is_empty() ->
    ets:first(i_events) == '$end_of_table'.

-spec free_vertex(Vertex :: binary()) -> ok.
free_vertex(Vertex) ->
    ets:update_element(i_state_of_queues, Vertex, {4, true}),
    get_dispatcher() ! {mark_for_scheduling, Vertex},
    ok.


%%%---------------------------
%% Dispatcher process
%%%---------------------------

dispatcher_routine() ->
    SetOfVertices = ets:lookup_element(i_dispatcher, ready_queues, 2),
    ets:update_element(i_dispatcher, ready_queues, {2, schedule_events(SetOfVertices)}),

    read_messages(),
    dispatcher_routine().


%%%---------------------------
%% Internal functions
%%%---------------------------

-spec do_post(Vertex :: binary(), Event :: event(), Notification :: notification()) -> ok.
do_post(Vertex, Event, Notification) ->
    ThisZone = gmm_utils:zone_id(),
    case gmm_utils:owner_of(Vertex) of
        ThisZone -> local_post(Vertex, Event, Notification);
        _Other -> outbox:post(Vertex, Event)
    end.

create_queue_if_absent(Vertex) ->
    ets:insert_new(i_state_of_queues, {Vertex, 0, 0, true}).

local_post(Vertex, Event, Notification) ->
    instrumentation:notify(Notification),

    case gmm_utils:get_indexation_enabled() of
        true ->
            create_queue_if_absent(Vertex),
            Idx = ets:update_counter(i_state_of_queues, Vertex, {3, 1}),
            ets:insert(i_events, {{Vertex, Idx}, Event}),
            case ets:lookup_element(i_state_of_queues, Vertex, 4) of
                true -> get_dispatcher() ! {mark_for_scheduling, Vertex}, ok;
                false -> ok
            end;
        false ->
            instrumentation:notify( notification:start_processing(Vertex, Event) ),
            instrumentation:notify( notification:end_processing(Vertex, Event) ),
            ok
    end.

get_dispatcher() ->
    ets:lookup_element(i_dispatcher, pid, 2).

mark_queue_as_ready_for_scheduling(Vertex) ->
    ReadyQueues = ets:lookup_element(i_dispatcher, ready_queues, 2),
    ets:update_element(i_dispatcher, ready_queues, {2, sets:add_element(Vertex, ReadyQueues)}).

look_for_eligible_queues() ->
    NewSet = ets:foldl(
        fun({Vertex, C1, C2, Free}, SetAcc) ->
            case Free and C1 < C2 of
                true -> sets:add_element(Vertex, SetAcc);
                false -> SetAcc
            end
        end, sets:new(), i_state_of_queues
    ),
    ets:update_element(i_dispatcher, ready_queues, {2, NewSet}).

schedule_events(Set) ->
    try
        sets:fold(
            fun(Vertex, SetAcc) ->
                Idx = ets:update_counter(i_state_of_queues, Vertex, {2, 1}),
                Event = ets:lookup_element(i_events, {Vertex, Idx}, 2),
                ets:update_element(i_state_of_queues, Vertex, {4, false}),
                io:format("Scheduling for vertex ~p. Event: \n~p\n", [Vertex, Event]),
                ets:delete(i_events, {Vertex, Idx}),
                sets:del_element(Vertex, SetAcc)
%%                case event_processor:can_schedule() of
%%                    true ->
%%                        Idx = ets:update_counter(i_state_of_queues, Vertex, {2, 1}),
%%                        Event = ets:lookup_element(i_events, {Vertex, Idx}, 2),
%%                        ets:update_element(i_state_of_queues, Vertex, {4, false}),
%%                        event_processor:schedule(Vertex, Event),
%%                        ets:delete(i_events, {Vertex, Idx}),
%%                        sets:del_element(Vertex, SetAcc);
%%                    false -> throw({break, SetAcc})
%%                end
            end, Set, Set
        )
    of
        NewSet -> NewSet
    catch
        throw:{break, NewSet} -> NewSet
    end.

read_messages() ->
    receive
        {mark_for_scheduling, Vertex} ->
            [{Vertex, C1, C2, Free}] = ets:lookup(i_state_of_queues, Vertex),
            case Free and (C1 < C2) of
                true -> mark_queue_as_ready_for_scheduling(Vertex);
                false -> ok
            end,
            read_messages()
    after 10 -> ok
    end.
