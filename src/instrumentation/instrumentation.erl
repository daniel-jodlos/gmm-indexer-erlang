%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that takes care of writing instrumentation to .csv file
%%% @end
%%%-------------------------------------------------------------------
-module(instrumentation).
-author("pawel").

%% API
-export([
    create_ets/0,
    start_link/0,
    init_writer/0,
    notify/1
]).

-define(MAX_BATCH_SIZE, 10000).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

create_ets() ->
    ets:new(instrumentation, [named_table, public]),
    ets:insert(instrumentation, {csv_writer, null}).

start_link() ->
    {ok, spawn_link(instrumentation, init_writer, [])}.

notify(Notification) ->
    ets:lookup_element(instrumentation, csv_writer, 2) ! Notification.


%%%---------------------------
%% Writer process routines
%%%---------------------------

read_blocking(File) ->
    receive
        Notification -> read_nonblocking(File, [Notification], 1)
    end.

read_nonblocking(File, Batch, ?MAX_BATCH_SIZE) ->
    write_batch(File, Batch);
read_nonblocking(File, Batch, Size) ->
    receive
        Notification -> read_nonblocking(File, [Notification | Batch], Size + 1)
    after 0 -> write_batch(File, Batch)
    end.

write_batch(File, Batch) ->
    ok = csv_writer:write_lines(File, Batch),
    read_blocking(File).


%%%---------------------------
%% Internal functions
%%%---------------------------

init_writer() ->
    ets:update_element(instrumentation, csv_writer, {2, self()}),
    {ok, File} = csv_writer:open_file(?CSV_FILE),
    read_blocking(File).
