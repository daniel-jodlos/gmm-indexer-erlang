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
    notify/1
]).

-define(MAX_BATCH_SIZE, 10000).

%%%---------------------------
%% Exported functions
%%%---------------------------

create_ets() ->
    ets:new(instrumentation, [named_table]),
    ets:insert(instrumentation, {csv_writer, null}).

start_link() ->
    {ok, spawn_link(instrumentation, init_writer, [])}.

notify(Notification) ->
    ets:lookup_element(instrumentation, csv_writer, 2) ! Notification.


%%%---------------------------
%% Writer process routines
%%%---------------------------

read_blocking() ->
    receive
        Notification -> read_nonblocking([Notification], 1)
    end.

read_nonblocking(Batch, ?MAX_BATCH_SIZE) ->
    write_batch(Batch, ?MAX_BATCH_SIZE);
read_nonblocking(Batch, Size) ->
    receive
        Notification -> read_nonblocking([Notification | Batch], Size + 1)
    after 0 -> write_batch(Batch, Size)
    end.

write_batch(Batch, Size) ->
    csv_writer:write_lines(Batch, Size),
    read_blocking().


%%%---------------------------
%% Internal functions
%%%---------------------------

init_writer() ->
    ets:update_element(instrumentation, csv_writer, {2, self()}),
    read_blocking().
