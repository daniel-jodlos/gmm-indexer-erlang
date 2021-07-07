%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that initializes outboxes and offers their functionalities
%%% @end
%%%-------------------------------------------------------------------
-module(outbox).
-author("pawel").

%% API
-export([
    specs_for_supervisor/0,
    init_outbox/1,
    post/2,
    is_empty/1,
    all_empty/0
]).


%%%---------------------------
%% Exported functions
%%%---------------------------

-spec specs_for_supervisor() -> list(map()).
specs_for_supervisor() ->
    lists:map(
        fun(Zone) -> #{
            id => << "outbox_", Zone/binary >>,
            start => {outbox, init_outbox, [Zone]}
        } end, gmm_utils:list_other_zones()
    ).

init_outbox(Zone) ->
    case ets:member(outboxes, Zone) of
        true -> ets:update_element(outboxes, Zone, {2, self()});
        false -> ets:insert(outboxes, {Zone, self(), []})
    end,
    timer:send_after(initial_delay(), send),
    outbox_routine(Zone, initial_delay()).

-spec post(Vertex :: binary(), Event :: map()) -> ok.
post(Vertex, Event) ->
    Zone = gmm_utils:owner_of(Vertex),
    outbox_pid(Zone) ! {event, {Vertex, Event}},
    ok.

-spec is_empty(Zone :: binary()) -> boolean().
is_empty(Zone) ->
    outbox_pid(Zone) ! {is_empty, self()},
    receive
        {is_empty, Bool} -> Bool
    after 1000 -> false
    end.

-spec all_empty() -> boolean().
all_empty() ->
    %% imitate ets:foreach
    Count = ets:foldl(
        fun({_, Pid, _, _}, Acc) ->
            Pid ! {is_empty, self()}, Acc + 1
        end, 0, outboxes),
    collect_answers(true, Count).


%%%---------------------------
%% Internal functions
%%%---------------------------

initial_delay() -> 10.
maximum_delay() -> 1000.
backoff_factor() -> 1.2.

-spec outbox_pid(Zone :: binary()) -> pid().
outbox_pid(Zone) ->
    ets:lookup_element(outboxes, Zone, 2).

%% @todo powinien wywalac sie ten kto zglasza event, nie outbox
-spec queue_event(Zone :: binary(), Vertex :: binary(), Event :: binary()) -> ok.
queue_event(Zone, Vertex, Event) ->
    {Zone, Name} = gmm_utils:split_vertex_id(Vertex),
    OldQueue = ets:lookup_element(outboxes, Zone, 3),
    ets:update_element(outboxes, Zone, {4, OldQueue ++ [{Name, Event}]}),
    ok.

-spec check_emptiness(Zone :: binary()) -> boolean().
check_emptiness(Zone) ->
    case ets:lookup_element(outboxes, Zone, 3) of
        [] -> true;
        _ -> false
    end.

-spec poll_batch(Zone :: binary()) -> {list(map()), list(map())}.
poll_batch(Zone) when is_binary(Zone) ->
    Events = ets:lookup_element(outboxes, Zone, 3),
    BatchSize = gmm_utils:batch_size(),
    case length(Events) of
        Small when Small =< BatchSize -> {Events, []};
        _Large -> lists:split(BatchSize, Events)
    end.

-spec try_sending(Zone :: binary(), Batch :: list(map())) -> ok | no_events | {error, any()}.
try_sending(_, Batch) when length(Batch) == 0 ->
    no_events;
try_sending(Zone, Batch) ->
    NameEventPairs = lists:map(
        fun({Vertex, Event}) ->
            {_, Name} = gmm_utils:split_vertex_id(Vertex),
            {Name, Event}
        end, Batch),
    MessagesList = lists:map(
        fun({Name, Event}) ->
            #{
                <<"vn">> => Name,
                <<"e">> => Event
            }
        end, NameEventPairs),
    BulkObject = #{<<"messages">> => MessagesList},
    zone_client:post_events(Zone, BulkObject).

-spec collect_answers(Acc :: boolean(), MsgToRead :: integer()) -> boolean().
collect_answers(Acc, 0) -> Acc;
collect_answers(Acc, MsgToRead) ->
    receive
        {is_empty, Bool} -> collect_answers(Acc and Bool, MsgToRead - 1)
    after 1000 -> false
    end.

%% Servant process

outbox_routine(Zone, Delay) ->
    receive
        {is_empty, Pid} ->
            Pid ! {is_empty, check_emptiness(Zone)},
            outbox_routine(Zone, Delay);
        send ->
            {Batch, Remaining} = poll_batch(Zone),
            NextDelay =
                case try_sending(Zone, Batch) of
                    ok ->
                        ets:update_element(outboxes, Zone, {3, Remaining}),
                        initial_delay();
                    no_events -> initial_delay();
                    {error, _} -> min( trunc(Delay * backoff_factor()), maximum_delay() )
                end,
            outbox_routine(Zone, NextDelay);
        {event, {Vertex, Event}} ->
            queue_event(Zone, Vertex, Event),
            outbox_routine(Zone, Delay)
    end.