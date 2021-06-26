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
    post_event/2, %% @todo ???
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

%%%---------------------------
%% Implementations
%%%---------------------------
% CONST
-define(URL, "localhost:8080/").
% TODO -> zones distinction, methods implementation, error handling, tests  

-spec healthcheck(Zone:: binary()) -> {ok, boolean()} | {error, any()}.
healthcheck(_Zone) ->
    {error, not_implemented}.

-spec index_ready(Zones:: binary() | list(binary())) -> {ok, boolean()} | {error, any()}.
index_ready(Zone) when is_binary(Zone) ->
    {error, not_implemented};

index_ready(AllZones) when is_list(AllZones) ->
    {error, not_implemented}.

% MUST
-spec is_adjacent(Zone:: binary(), From:: binary(), To:: binary()) -> {ok, boolean()} | {error, any()}.
is_adjacent(_Zone, _From, _To) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Url = http_utils:build_url(Address, <<"is_adjacent">>, [{<<"from">>, _From}, {<<"to">>, _To}]),
    {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
    if ResultNumber < 400 ->
      {ok, binary_to_atom(ResultMessage, utf8)};
    ResultNumber > 399 ->
      {error, ResultMessage}
  end.
% SHOULD POTENTIAL
-spec list_adjacent(Zone:: binary(), Of:: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent(_Zone, _Of) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent">>, [{<<"of">>, _Of}]),
    {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
    if ResultNumber < 400 ->
      {ok, binary_to_list(ResultMessage)};
    ResultNumber > 399 ->
      {error, ResultMessage}
  end.

% SHOULD POTENTIAL
-spec list_adjacent_reversed(Zone:: binary(), Of:: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent_reversed(_Zone, _Of) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent_reversed">>, [{<<"of">>, _Of}]),
    {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
    if ResultNumber < 400 ->
      {ok, binary_to_list(ResultMessage)};
    ResultNumber > 399 ->
      {error, ResultMessage}
  end.

% MUST POTENTIAL
-spec permissions(Zone::binary(), From:: binary(), To:: binary()) -> {ok, binary()} | {error, any()}.
permissions(_Zone, _From, _To) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Url = http_utils:build_url(Address, <<"permissions">>, [{<<"from">>, _From}, {<<"to">>, _To}]),
    {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
    if ResultNumber < 400 ->
      {ok, ResultMessage};
    ResultNumber > 399 ->
      {error, ResultMessage}
  end.
% MUST
-spec add_edge(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: binary(), Trace:: binary()
    ) -> ok | {error, any()}.
add_edge(_Zone, _From, _To, _Permissions, _Trace) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Params = [{<<"from">>, _From}, {<<"to">>, _To}, {<<"permissions">>, _Permissions}, {<<"successive">>, <<"false">>}]
    ++ (case _Trace of undefined -> []; _ -> [{<<"trace">>, _Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges">>, Params),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during adding edge"}
    end.

% MUST
-spec add_edge(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: binary(), Trace:: binary(),
    Successive:: boolean()) -> ok | {error, any()}.
add_edge(_Zone, _From, _To, _Permissions, _Trace, _Successive) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Params = [{<<"from">>, _From}, {<<"to">>, _To}, {<<"permissions">>, _Permissions}, {<<"successive">>, atom_to_binary(_Successive, utf8)}]
      ++ (case _Trace of undefined -> []; _ -> [{<<"trace">>, _Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges">>, Params),{ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during adding edge"}
    end.

-spec add_edges(Zone:: binary(), BulkRequest:: binary()) -> ok | {error, any()}.
add_edges(_Zone, _BulkRequest) ->
    {error, not_implemented}.

% MUST
-spec remove_edge(Zone:: binary(), From:: binary(), To:: binary(), Trace:: binary()) -> ok | {error, any()}.
remove_edge(Zone, From, To, Trace) ->
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}]
      ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/delete">>, Params),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during adding edge"}
    end.

% MUST
-spec remove_edge(Zone:: binary(), From:: binary(), To:: binary(), Trace:: binary(),
    Successive:: boolean()) -> ok | {error, any()}.
remove_edge(_Zone, _From, _To, _Trace, _Successive) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Params = [{<<"from">>, _From}, {<<"to">>, _To}, {<<"successive">>, atom_to_binary(_Successive, utf8)}]
      ++ (case _Trace of undefined -> []; _ -> [{<<"trace">>, _Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/delete">>, Params),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during removing edge"}
    end.

% MUST
-spec set_permissions(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: binary(),
    Trace:: binary()) -> ok | {error, any()}.
set_permissions(Zone, From, To, Permissions, Trace) ->
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"permissions">>, Permissions}]
      ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/permissions">>, Params),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during setting permissions"}
    end.

% MUST
-spec set_permissions(Zone:: binary(), From:: binary(), To:: binary(), Permissions:: binary(),
    Trace:: binary(), Successive:: boolean()) -> ok | {error, any()}.
set_permissions(_Zone, _From, _To, _Permissions, _Trace, _Successive) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Params = [{<<"from">>, _From}, {<<"to">>, _To}, {<<"permissions">>, _Permissions}, {<<"successive">>, atom_to_binary(_Successive, utf8)}]
      ++ (case _Trace of undefined -> []; _ -> [{<<"trace">>, _Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/permissions">>, Params),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during setting permissions"}
    end.

% MUST
-spec add_vertex(Zone:: binary(), Name:: binary(), Type:: binary()) -> ok | {error, any()}.
add_vertex(_Zone, _Name, _Type) ->
    {ok, Address} = http_utils:get_address(_Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices">>,
      [{<<"type">>, _Type}, {<<"name">>, _Name}]),
    {ok, ResultNumber, _, _} = requests:post_request(Url),
    if ResultNumber < 400 ->
        ok;
      ResultNumber > 399 ->
        {error, "Error Occured during adding vertex"}
    end.

-spec add_vertices(Zone:: binary(), BulkRequest:: map()) -> ok | {error, any()}.
add_vertices(_Zone, _BulkRequest) ->
    {error, not_implemented}.

-spec post_event(VertexId:: binary(), Event:: map()) -> ok | {error, any()}.
post_event(_VertexId, _Event) ->
    {error, not_implemented}.

-spec post_events(Zone:: binary(), BulkMessages:: map()) -> ok | {error, any()}.
post_events(_Zone, _BulkMessages) ->
    {error, not_implemented}.

-spec get_event_stats(Zone:: binary()) -> {ok, map()} | {error, any()}.
get_event_stats(_Zone) ->
    {error, not_implemented}.

-spec get_dependent_zones(Zone:: binary()) -> {ok, map()} | {error, any()}.
get_dependent_zones(Zone) ->
    get_dependent_zones(Zone, []).

-spec get_dependent_zones(Zone:: binary(), ToExclude:: list(binary())) -> {ok, map()} | {error, any()}.
get_dependent_zones(_Zone, _ToExclude) ->
    {error, not_implemented}.

-spec is_instrumentation_enabled(Zone:: binary()) -> {ok, boolean()} | {error, any()}.
is_instrumentation_enabled(_Zone) ->
    {error, not_implemented}.

-spec set_instrumentation_enabled(Zone:: binary(), Enabled:: boolean()) -> ok | {error, any()}.
set_instrumentation_enabled(_Zone, _Enabled) ->
    {error, not_implemented}.

-spec set_indexation_enabled(Zone:: binary(), Enabled:: boolean()) -> ok | {error, any()}.
set_indexation_enabled(_Zone, _Enabled) ->
    {error, not_implemented}.

-spec simulate_load(Zone:: binary(), LoadRequest:: map()) -> ok | {error, any()}.
simulate_load(_Zone, _LoadRequest) ->
    {error, not_implemented}.

%% function that blocks process until zone (all zones) returns 'true' from 'IP/index_ready' endpoint
%%  - delay between consecutive requests may be constant, or Exponential Backoff can be used, whatever
-spec wait_for_index(Zones:: binary() | list(binary()), Timeout:: integer()) -> ok | {error, any()}.
wait_for_index(Zone, _Timeout) when is_binary(Zone) ->
    {error, not_implemented};

wait_for_index(Zones, _Timeout) when is_list(Zones) ->
    {error, not_implemented}.

% MUST POTENTIAL
-spec reaches(Algo:: naive | reaches, Zone:: binary(), From:: binary(), To:: binary()
    ) -> {ok, map()} | {error, any()}.
reaches(_Algo, _Zone, _From, _To) ->
    case _Algo of
      naive ->
        {ok, Address} = http_utils:get_address(_Zone),
        Url = http_utils:build_url(Address, <<"naive/reaches">>, [{<<"from">>, _From}, {<<"to">>, _To}]),
        {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
        if ResultNumber < 400 ->
          {ok, json_utils:decode(ResultMessage)};
          ResultNumber > 399 ->
            {error, ResultMessage}
        end;

      reaches ->
        {error, not_implemented}
    end.


% MUST POTENTIAL
-spec members(Algo:: naive | reaches, Zone:: binary(), Of:: binary()) -> {ok, map()} | {error, any()}.
members(_Algo, _Zone, _Of) ->
  case _Algo of
    naive ->
      {ok, Address} = http_utils:get_address(_Zone),
      Url = http_utils:build_url(Address, <<"naive/members">>, [{<<"of">>, _Of}]),
      {ResultNumber, ResultMessage} = requests:post_request_with_response_body(Url),
      if ResultNumber < 400 ->
        {ok, json_utils:decode(ResultMessage)};
        ResultNumber > 399 ->
          {error, ResultMessage}
      end;

    reaches ->
      {error, not_implemented}
  end.

-spec effective_permissions(Algo:: naive | reaches, Zone:: binary(), From:: binary(),
    To:: binary()) -> {ok, map()} | {error, any()}.
effective_permissions(_Algo, _Zone, _From, _To) ->
    {error, not_implemented}.
