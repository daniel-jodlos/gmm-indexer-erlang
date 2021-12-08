%%%-------------------------------------------------------------------
%%% @doc
%%%  Quick module to save and read settings
%%% @end
%%%-------------------------------------------------------------------
-module(settings).
-author("pawel").

%% API
-export([
    create_ets/0,

    set_indexation_enabled/1,
    get_indexation_enabled/0,
    set_instrumentation_enabled/1,
    get_instrumentation_enabled/0,

    set_events_batch_size/1,
    get_events_batch_size/0,

    get_local_tests/0
]).

-define(TABLE, settings).

-define(INDEXATION, indexation).
-define(INSTRUMENTATION, instrumentation).
-define(EVENTS_BATCH_SIZE, e_batch_size).
-define(LOCAL_TESTS, local_tests).


%%%---------------------------
%% Exported functions
%%%---------------------------

create_ets() ->
    LocalTests = os:getenv("LOCAL_TESTS") =/= false,

    ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    ets:insert(?TABLE, {?INDEXATION, true}),
    ets:insert(?TABLE, {?INSTRUMENTATION, true}),
    ets:insert(?TABLE, {?EVENTS_BATCH_SIZE, 5}),
    ets:insert(?TABLE, {?LOCAL_TESTS, LocalTests}).


set_indexation_enabled(Bool) when is_boolean(Bool) ->
    ets:update_element(?TABLE, ?INDEXATION, {2, Bool}).

get_indexation_enabled() ->
    ets:lookup_element(?TABLE, ?INDEXATION, 2).

set_instrumentation_enabled(Bool) when is_boolean(Bool) ->
    ets:update_element(?TABLE, ?INSTRUMENTATION, {2, Bool}).

get_instrumentation_enabled() ->
    ets:lookup_element(?TABLE, ?INSTRUMENTATION, 2).


set_events_batch_size(Size) when is_integer(Size), Size > 0 ->
    ets:update_element(?TABLE, ?EVENTS_BATCH_SIZE, {2, Size}).

get_events_batch_size() ->
    ets:lookup_element(?TABLE, ?EVENTS_BATCH_SIZE, 2).


get_local_tests() ->
    ets:lookup_element(?TABLE, ?LOCAL_TESTS, 2).
