%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that takes care of writing instrumentation to .csv file
%%% @end
%%%-------------------------------------------------------------------
-module(instrumentation).
-author("pawel").

%% API
-export([
    start_link/0,
    init_writer/0,
    notify/1
]).

-define(MAX_BATCH_SIZE, 10000).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

start_link() ->
    {ok, spawn_link(instrumentation, init_writer, [])}.

notify(Notification) ->
    whereis(instrumentation_process) ! Notification.


%%%---------------------------
%% Writer process routines
%%%---------------------------

writer_loop(File, [], 0) ->
    receive
        Notification -> writer_loop(File, [Notification], 1)
    end;
writer_loop(File, Batch, ?MAX_BATCH_SIZE) ->
    ok = csv_writer:write_lines(File, Batch),
    writer_loop(File, [], 0);
writer_loop(File, Batch, Size) ->
    receive
        Notification -> writer_loop(File, [Notification | Batch], Size + 1)
    after 0 ->
        ok = csv_writer:write_lines(File, Batch),
        writer_loop(File, [], 0)
    end.


%%%---------------------------
%% Internal functions
%%%---------------------------

init_writer() ->
    register(instrumentation_process, self()),
    {ok, File} = csv_writer:open_file(?CSV_FILE),
    writer_loop(File, [], 0).
