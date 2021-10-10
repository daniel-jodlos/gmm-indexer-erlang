%%%-------------------------------------------------------------------
%%% @doc
%%%  Module to create and manipulate notification objects
%%%  All notifications are about events, which I left implicit
%%% @end
%%%-------------------------------------------------------------------
-module(notification).
-author("pawel").

%% API
-export([
    queue/2,
    start_processing/2,
    end_processing/2,
    fail_processing/2,
    fork/3,
    mark_dirty/2,
    mark_clean/2
]).

-include("records.hrl").

%%%---------------------------
%% Exported functions
%%%---------------------------

queue(VertexId, Event) ->
    base_notification(<<"queue">>, VertexId, Event).

start_processing(VertexId, Event) ->
    base_notification(<<"start">>, VertexId, Event).

end_processing(VertexId, Event) ->
    base_notification(<<"end">>, VertexId, Event).

fail_processing(VertexId, Event) ->
    base_notification(<<"fail">>, VertexId, Event).

fork(VertexId, Event, Children) ->
    BaseNotification = base_notification(<<"fork">>, VertexId, Event),
    maps:update(fork_children, Children, BaseNotification).

mark_dirty(VertexId, Event) ->
    base_notification(<<"dirty">>, VertexId, Event).

mark_clean(VertexId, Event) ->
    base_notification(<<"clean">>, VertexId, Event).


%%%---------------------------
%% Internal functions
%%%---------------------------

-spec base_notification(binary(), binary(), event()) -> notification().
base_notification(NotificationType, VertexId, _Event = #{<<"id">> := EventId,
    <<"trace">> := Trace, <<"type">> := EventType, <<"sender">> := Sender, <<"originalSender">> := OriginalSender}) ->
    #{
        zone            => gmm_utils:zone_id(),
        time            => gmm_utils:nanosecond_timestamp_to_iso6801( erlang:system_time(nanosecond) ),
        thread          => list_to_binary( pid_to_list(self()) ),
        notif_type      => NotificationType,
        trace           => Trace,
        event_id        => EventId,
        event_type      => EventType,
        vertex          => VertexId,
        sender          => Sender,
        original_sender => OriginalSender,
        fork_children   => 0
    }.
