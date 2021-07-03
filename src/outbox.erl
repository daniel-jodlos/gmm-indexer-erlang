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
        false -> ets:insert(outboxes, {Zone, self(), initial_delay(), []})
    end,
    timer:send_after(initial_delay(), send),
    outbox_routine(Zone).

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

-spec queue_event(Zone :: binary(), Vertex :: binary(), Event :: binary()) -> ok.
queue_event(Zone, Vertex, Event) ->
    {Zone, Name} = gmm_utils:split_vertex_id(Vertex),
    OldQueue = ets:lookup_element(outboxes, Zone, 4),
    ets:update_element(outboxes, Zone, {4, OldQueue ++ [{Name, Event}]}),
    ok.

-spec check_emptiness(Zone :: binary()) -> boolean().
check_emptiness(Zone) ->
    case ets:lookup_element(outboxes, Zone, 4) of
        [] -> true;
        _ -> false
    end.

-spec poll_batch(Events :: list(map())) -> {list(map()), list(map())}.
poll_batch(Events) ->
    BatchSize = gmm_utils:batch_size(),
    case length(Events) of
        Small when Small =< BatchSize -> {Events, []};
        _Large -> lists:split(BatchSize, Events)
    end.

-spec try_sending(Zone :: binary(), Batch :: list(map()), Remaining :: list(map())) -> {list(map()), integer()}.
try_sending(_, [], Remaining) ->
    {Remaining, initial_delay()};
try_sending(Zone, Batch, Remaining) ->
    MessagesList = lists:map(
        fun({Name, Event}) ->
            #{
                <<"vn">> => Name,
                <<"e">> => Event
            }
        end, Batch),
    BulkObject = #{<<"messages">> => MessagesList},
    case zone_client:post_events(Zone, BulkObject) of
        ok -> {Remaining, initial_delay()};
        {error, _} ->
            CurrDelay = ets:lookup_element(outboxes, Zone, 3),
            NextDelay = min( trunc(CurrDelay * backoff_factor()), maximum_delay() ),
            {Batch ++ Remaining, NextDelay}
    end.

-spec collect_answers(Acc :: boolean(), MsgToRead :: integer()) -> boolean().
collect_answers(Acc, 0) -> Acc;
collect_answers(Acc, MsgToRead) ->
    receive
        {is_empty, Bool} -> collect_answers(Acc and Bool, MsgToRead - 1)
    after 1000 -> false
    end.

%% Servant process

outbox_routine(Zone) ->
    receive
        {is_empty, Pid} ->
            Pid ! {is_empty, check_emptiness(Zone)};
        send ->
            {Batch, Remaining} = poll_batch(ets:lookup_element(outboxes, Zone, 4)),
            {FinalQueue, Delay} = try_sending(Zone, Batch, Remaining),
            ets:update_element(outboxes, Zone, [{3, Delay}, {4, FinalQueue}]),
            timer:send_after(Delay, send);
        {event, {Vertex, Event}} ->
            queue_event(Zone, Vertex, Event)
    end,
    outbox_routine(Zone).
