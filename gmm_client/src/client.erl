%%%-------------------------------------------------------------------
%%% @doc
%%%  Functions executing requests to public API offered by zones
%%% @end
%%%-------------------------------------------------------------------
-module(client).
-author("pawel").

%% API
-export([
    add_vertex/3,
    get_all_vertices/1,
    get_vertex/1,
    delete_vertex/1,
    add_edge/4,
    set_permissions/4,
    delete_edge/3,
    edge_exists/2,
    get_permissions/2,
    list_parents/1,
    list_children/1,
    naive_members/1,
    naive_reaches/2,
    naive_effective_permissions/2,
    indexed_members/1,
    indexed_reaches/2,
    indexed_effective_permissions/2
]).


%%%---------------------------
%% Implementations
%%%---------------------------

%% VERTICES

-spec add_vertex(Zone :: binary(), Type :: binary(), Name :: binary()) -> ok | {error, any()}.
add_vertex(Zone, Type, Name) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices">>,
        [{<<"type">>, Type}, {<<"name">>, Name}]),
    http_executor:post(Url, false).

-spec get_all_vertices(Zone :: binary()) -> {ok, map()} | {error, any()}.
get_all_vertices(Zone) ->
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices/listing">>),
    http_executor:get(Url).

-spec get_vertex(Id :: binary()) -> {ok, map()} | {error, any()}.
get_vertex(Id) ->
    Zone = client_utils:owner_of(Id),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices/details">>, [{<<"id">>, Id}]),
    http_executor:get(Url).

-spec delete_vertex(Id :: binary()) -> ok | {error, any()}.
delete_vertex(Id) ->
    Zone = client_utils:owner_of(Id),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"graph/vertices/delete">>, [{<<"id">>, Id}]),
    http_executor:post(Url, false).

%% EDGES

-spec add_edge(From :: binary(), To :: binary(), Permissions :: binary(), Trace :: binary() | undefined) ->
    ok | {error, any()}.
add_edge(From, To, Permissions, Trace) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"permissions">>, Permissions}, {<<"successive">>, <<"false">>}]
        ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges">>, Params),
    http_executor:post(Url, false).

-spec set_permissions(From :: binary(), To :: binary(), Permissions :: binary(), Trace :: binary() | undefined) ->
    ok | {error, any()}.
set_permissions(From, To, Permissions, Trace) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"permissions">>, Permissions}, {<<"successive">>, <<"false">>}]
        ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/permissions">>, Params),
    http_executor:post(Url, false).

-spec delete_edge(From :: binary(), To :: binary(), Trace :: binary() | undefined) -> ok | {error, any()}.
delete_edge(From, To, Trace) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Params = [{<<"from">>, From}, {<<"to">>, To}, {<<"successive">>, <<"false">>}]
        ++ (case Trace of undefined -> []; _ -> [{<<"trace">>, Trace}] end),
    Url = http_utils:build_url(Address, <<"graph/edges/delete">>, Params),
    http_executor:post(Url, false).

-spec edge_exists(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
edge_exists(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"is_adjacent">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

-spec get_permissions(From :: binary(), To :: binary()) -> {ok, binary()} | {error, any()}.
get_permissions(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"permissions">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

-spec list_parents(Of :: binary()) -> {ok, list(binary())} | {error, any()}.
list_parents(Of) ->
    Zone = client_utils:owner_of(Of),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

-spec list_children(Of :: binary()) -> {ok, list(binary())} | {error, any()}.
list_children(Of) ->
    Zone = client_utils:owner_of(Of),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"list_adjacent_reversed">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

-spec naive_members(Of :: binary()) -> {ok, map()} | {error, any()}.
naive_members(Of) ->
    Zone = client_utils:owner_of(Of),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"naive/members">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

-spec naive_reaches(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
naive_reaches(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"naive/reaches">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

-spec naive_effective_permissions(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
naive_effective_permissions(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"naive/effective_permissions">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

-spec indexed_members(Of :: binary()) -> {ok, map()} | {error, any()}.
indexed_members(Of) ->
    Zone = client_utils:owner_of(Of),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"indexed/members">>, [{<<"of">>, Of}]),
    http_executor:post(Url, true).

-spec indexed_reaches(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
indexed_reaches(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"indexed/reaches">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).

-spec indexed_effective_permissions(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
indexed_effective_permissions(From, To) ->
    Zone = client_utils:owner_of(From),
    {ok, Address} = http_utils:get_address(Zone),
    Url = http_utils:build_url(Address, <<"indexed/effective_permissions">>, [{<<"from">>, From}, {<<"to">>, To}]),
    http_executor:post(Url, true).