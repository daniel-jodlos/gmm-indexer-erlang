%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that takes care of logic of writing notifications to .csv
%%% @end
%%%-------------------------------------------------------------------
-module(csv_writer).
-author("pawel").

%% API
-export([
    open_file/1,
    write_lines/2
]).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

-spec open_file(Name :: iodata()) -> {ok, file:io_device()} | {error, any()}.
open_file(FileName) ->
    file:open(FileName, [append, raw]).

-spec write_lines(file:io_device(), list(notification())) -> ok | {error, any()}.
write_lines(File, Notifications) ->
    FinalBinary = lists:foldl(
        fun( #{
            zone := Zone,
            time := Time,
            thread := Thread,
            notif_type := NotifType,
            trace := Trace,
            event_id := EventId,
            event_type := EventType,
            vertex := VertexId,
            sender := Sender,
            original_sender := OriginalSender,
            fork_children := ForkChildren
        }, AccBinary) ->
            <<
                AccBinary/binary,
                "\"", Zone/binary, "\",",
                "\"", Time/binary, "\",",
                "\"", Thread/binary, "\",",
                "\"", NotifType/binary, "\",",
                "\"", Trace/binary, "\",",
                "\"", EventId/binary, "\",",
                "\"", EventType/binary, "\",",
                "\"", VertexId/binary, "\",",
                "\"", Sender/binary, "\",",
                "\"", OriginalSender/binary, "\",",
                "\"", (integer_to_binary(ForkChildren))/binary, "\"\n"
            >>
        end, <<"">>, Notifications
    ),
    file:write(File, FinalBinary).
