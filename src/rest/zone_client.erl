%%%-------------------------------------------------------------------
%% @doc
%%  Enables zones to communicate with each other
%%
%%  Declared functions are assigned one out of 5 categories:
%%   1. necessary -- necessary to make naive engine
%%   2. important -- maybe necessary, I'm not sure yet
%%   3. optional  -- maybe useful, but I wouldn't focus on them
%%   4. ignore    -- they will be needed for 'indexed' version
%%   5. ???       -- I'm not sure if they need to exist at all
%% @end
%%%-------------------------------------------------------------------

-module(zone_client).
-author("pawel").

%% API
-export([
    healthcheck/1, %% @todo optional
    index_ready/1, %% @todo ignore
    is_adjacent/3, %% @todo necessary
    list_adjacent/2, %% @todo important
    list_adjacent_reversed/2, %% @todo important
    permissions/3, %% @todo necessary
    add_edge/5,
    add_edge/6, %% @todo necessary
    add_edges/2, %% @todo optional
    remove_edge/4, %% @todo necessary
    remove_edge/5, %% @todo necessary
    set_permissions/5, %% @todo necessary
    set_permissions/6, %% @todo necessary
    add_vertex/3, %% @todo necessary
    add_vertices/2, %% @todo optional
    post_event/3, %% @todo ???
    post_events/2, %% @todo optional
    get_event_stats/1, %% @todo ignore
    get_dependent_zones/1, %% @todo necessary later now optional
    get_dependent_zones/2, %% @todo necessary later now optional
    is_instrumentation_enabled/1, %% @todo ???
    set_instrumentation_enabled/2, %% @todo ???
    set_indexation_enabled/2, %% @todo ignore
    simulate_load/2, %% @todo ignore
    wait_for_index/2, %% @todo ignore
    reaches/4, %% @todo naive -> necessary, indexed -> ignore
    members/3, %% @todo naive -> necessary, indexed -> ignore
    effective_permissions/4 %% @todo naive -> necessary, indexed -> ignore
]).

-include("records.hrl").


%%%---------------------------
%% Implementations
%%%---------------------------

-spec healthcheck(Zone:: binary()) -> ok | {error, any()}.
healthcheck(Zone) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"healthcheck">>),
    case http_executor:get(Url) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec index_ready
    (Zone :: binary()) -> {ok, boolean()} | {error, any()};
    (Zones:: list(binary())) -> {ok, boolean()} | {error, any()}.
index_ready(Zone) when is_binary(Zone) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"index_ready">>),
    http_executor:get(Url);

index_ready(Zones) when is_list(Zones) ->
    %% todo execute those requests in parallel
    Results = lists:map(fun index_ready/1, Zones),
    lists:foldl(
        fun
            (_, {error, R}) -> {error, R};
            ({error, R}, _) -> {error, R};
            ({ok, Bool}, {ok, Acc}) -> {ok, Bool and Acc}
        end,
        {ok, true},
        Results
    ).

% MUST
-spec is_adjacent(Zone:: binary(), From:: binary(), To:: binary()) -> {ok, boolean()} | {error, any()}.
is_adjacent(Zone, From, To) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"is_adjacent">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

% SHOULD POTENTIAL
-spec list_adjacent(Zone:: binary(), Of:: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent(Zone, Of) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

% SHOULD POTENTIAL
-spec list_adjacent_reversed(Zone:: binary(), Of:: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent_reversed(Zone, Of) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent_reversed">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

% MUST POTENTIAL
-spec permissions(Zone::binary(), From:: binary(), To:: binary()) -> {ok, binary()} | {error, any()}.
permissions(Zone, From, To) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"permissions">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

% MUST
-spec add_edge(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: permissions(), Trace:: binary()
    ) -> ok | {error, any()}.
add_edge(Zone, From, To, Permissions, Trace) ->
    add_edge(Zone, From, To, Permissions, Trace, false).

% MUST
-spec add_edge(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: permissions(), Trace:: binary(),
    Successive:: boolean()) -> ok | {error, any()}.
add_edge(Zone, From, To, Permissions, Trace, Successive) ->
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"permissions">>, Permissions}, {<<"successive">>, atom_to_binary(Successive, utf8)}]
      ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges">>, Params),
    http_executor:post(Url, false).

-spec add_edges(Zone:: binary(), BulkRequest:: binary()) -> ok | {error, any()}.
add_edges(Zone, BulkRequest) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/edges/bulk">>,[]),
    http_executor:post(Url, BulkRequest, false).

% MUST
-spec remove_edge(Zone:: binary(), From:: binary(), To:: binary(), Trace:: binary()) -> ok | {error, any()}.
remove_edge(Zone, From, To, Trace) ->
    remove_edge(Zone, From, To, Trace, false).

% MUST
-spec remove_edge(Zone:: binary(), From:: binary(), To:: binary(), Trace:: binary(),
    Successive:: boolean()) -> ok | {error, any()}.
remove_edge(Zone, From, To, Trace, Successive) ->
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"successive">>, atom_to_binary(Successive, utf8)}]
      ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/delete">>, Params),
    http_executor:post(Url, false).

% MUST
-spec set_permissions(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: permissions(),
    Trace:: binary()) -> ok | {error, any()}.
set_permissions(Zone, From, To, Permissions, Trace) ->
    set_permissions(Zone, From, To, Permissions, Trace, false).

% MUST
-spec set_permissions(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: permissions(),
    Trace:: binary(), Successive:: boolean()) -> ok | {error, any()}.
set_permissions(Zone, From, To, Permissions, Trace, Successive) ->
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"permissions">>, Permissions}, {<<"successive">>, atom_to_binary(Successive, utf8)}]
      ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/permissions">>, Params),
    http_executor:post(Url, false).

% MUST
-spec add_vertex(Zone:: binary(), Name:: binary(), Type:: binary()) -> ok | {error, any()}.
add_vertex(Zone, Name, Type) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices">>,
      [{<<"type">>, Type}, {<<"name">>, Name}]),
    http_executor:post(Url, false).

-spec add_vertices(Zone:: binary(), BulkRequest:: map()) -> ok | {error, any()}.
add_vertices(Zone, BulkRequest) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices/bulk">>,[]),
    http_executor:post(Url, BulkRequest, false).

-spec post_event(Zone:: binary(), VertexId:: binary(), Event:: map()) -> ok | {error, any()}.
post_event(Zone, VertexId, Event) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"events">>, [{<<"id">>, VertexId}]),
    http_executor:post(Url, Event, false).

-spec post_events(Zone:: binary(), BulkMessages:: map()) -> ok | {error, any()}.
post_events(Zone, BulkMessages) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"events/bulk">>,[]),
    http_executor:post(Url, BulkMessages, false).

-spec get_event_stats(Zone:: binary()) -> {ok, map()} | {error, any()}.
get_event_stats(Zone) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"events/stats">>, []),
    http_executor:get(Url).

-spec get_dependent_zones(Zone:: binary()) -> {ok, map()} | {error, any()}.
get_dependent_zones(Zone) ->
    get_dependent_zones(Zone, []).

-spec get_dependent_zones(Zone:: binary(), ToExclude:: list(binary())) -> {ok, map()} | {error, any()}.
get_dependent_zones(Zone, ToExclude) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"dependent_zones">>),
    http_executor:post(Url, ToExclude, true).

-spec is_instrumentation_enabled(Zone:: binary()) -> {ok, boolean()} | {error, any()}.
is_instrumentation_enabled(Zone) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"instrumentation">>),
    http_executor:get(Url).

-spec set_instrumentation_enabled(Zone:: binary(), Enabled:: boolean()) -> ok | {error, any()}.
set_instrumentation_enabled(Zone, Enabled) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"instrumentation">>),
    http_executor:put(Url, Enabled).

-spec set_indexation_enabled(Zone:: binary(), Enabled:: boolean()) -> ok | {error, any()}.
set_indexation_enabled(Zone, Enabled) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"indexation">>),
    http_executor:put(Url, Enabled).

-spec simulate_load(Zone:: binary(), LoadRequest:: map()) -> ok | {error, any()}.
simulate_load(Zone, LoadRequest) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"simulate_load">>,[]),
    http_executor:post(Url, false).

%% function that blocks process until zone (all zones) returns 'true' from 'IP/index_ready' endpoint
%%  - delay between consecutive requests may be constant, or Exponential Backoff can be used, whatever
-spec wait_for_index(Zones:: binary() | list(binary()), Timeout:: integer()) -> ok | {error, any()}.
wait_for_index(Zone, Timeout) when is_binary(Zone) ->
    {error, not_implemented};

wait_for_index(Zones, Timeout) when is_list(Zones) ->
    {error, not_implemented}.

% MUST POTENTIAL
-spec reaches(Algo:: naive | indexed, Zone:: binary(), From:: binary(), To:: binary()
    ) -> {ok, map()} | {error, any()}.
reaches(Algo, Zone, From, To) ->
    Path = case Algo of
             naive -> <<"naive/reaches">>;
             indexed -> <<"indexed/reaches">>
           end,
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, Path, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).


% MUST POTENTIAL
-spec members(Algo:: naive | indexed, Zone:: binary(), Of:: binary()) -> {ok, map()} | {error, any()}.
members(Algo, Zone, Of) ->
    Path = case Algo of
            naive -> <<"naive/members">>;
            indexed -> <<"indexed/members">>
          end,
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, Path, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

-spec effective_permissions(Algo:: naive | reaches, Zone:: binary(), From:: binary(),
    To:: binary()) -> {ok, map()} | {error, any()}.
effective_permissions(Algo, Zone, From, To) ->
    Path = case Algo of
               naive -> <<"naive/effective_permissions">>;
               indexed -> <<"indexed/effective_permissions">>
           end,
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, Path, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).
