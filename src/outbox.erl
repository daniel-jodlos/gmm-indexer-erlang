%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that initializes outboxes and offers their functionalities
%%% @end
%%%-------------------------------------------------------------------
-module(outbox).
-author("pawel").

%% API
-export([
    create_ets/0,
    specs_for_supervisor/0,
    start_link/1,
    init_outbox/1,
    post/2,
    is_empty/1,
    all_empty/0
]).

-define(INITIAL_DELAY, 10).
-define(MAXIMUM_DELAY, 1000).
-define(BACKOFF_FACTOR, 1.2).


%%%---------------------------
%% Exported functions
%%%---------------------------

create_ets() ->
    ets:new(outboxes, [named_table, public]).

-spec specs_for_supervisor() -> list(map()).
specs_for_supervisor() ->
    lists:map(
        fun(Zone) -> #{
            id => << "outbox_", Zone/binary >>,
            start => {outbox, start_link, [Zone]}
        } end, gmm_utils:list_other_zones()
    ).

start_link(Zone) ->
    case ets:member(outboxes, Zone) of
        true ->
            ets:update_element(outboxes, Zone, {2, null});
        false ->
            ets:insert(outboxes, {Zone, null, []})
    end,
    {ok, spawn_link(outbox, init_outbox, [Zone])}.

init_outbox(Zone) ->
    ets:update_element(outboxes, Zone, {2, self()}),
    timer:send_after(?INITIAL_DELAY, send),
    outbox_routine(Zone, ?INITIAL_DELAY).

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
        fun({_, Pid, _}, Acc) ->
            Pid ! {is_empty, self()}, Acc + 1
        end, 0, outboxes),
    collect_answers(true, Count).


%%%---------------------------
%% Internal functions
%%%---------------------------

-spec outbox_pid(Zone :: binary()) -> pid().
outbox_pid(Zone) ->
    ets:lookup_element(outboxes, Zone, 2).

%% @todo powinien wywalac sie ten kto zglasza event, nie outbox
-spec queue_event(Zone :: binary(), Vertex :: binary(), Event :: binary()) -> ok.
queue_event(Zone, Vertex, Event) ->
    {Zone, Name} = gmm_utils:split_vertex_id(Vertex),
    OldQueue = ets:lookup_element(outboxes, Zone, 3),
    ets:update_element(outboxes, Zone, {3, OldQueue ++ [{Name, Event}]}),
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
    BatchSize = settings:get_events_batch_size(),
    case length(Events) of
        Small when Small =< BatchSize -> {Events, []};
        _Large -> lists:split(BatchSize, Events)
    end.

-spec try_sending(Zone :: binary(), Batch :: list(map())) -> ok | no_events | {error, any()}.
try_sending(_, Batch) when length(Batch) == 0 ->
    no_events;
try_sending(Zone, Batch) ->
    MessagesList = lists:map(
        fun({Name, Event}) ->
            #{
                <<"vn">> => Name,
                <<"e">> => maps:remove(<<"id">>, Event)
            }
        end, Batch),
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
                        ?INITIAL_DELAY;
                    no_events -> ?INITIAL_DELAY;
                    {error, _} -> min( trunc(Delay * ?BACKOFF_FACTOR), ?MAXIMUM_DELAY )
                end,
            timer:send_after(NextDelay, send),
            outbox_routine(Zone, NextDelay);
        {event, {Vertex, Event}} ->
            queue_event(Zone, Vertex, Event),
            outbox_routine(Zone, Delay)
    end.
