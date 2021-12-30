%%%-------------------------------------------------------------------
%% @doc
%%  Helper module to load graph from JSON file
%% @end
%%%-------------------------------------------------------------------

-module(test_utils).
-include_lib("stdlib/include/assert.hrl").

%% API
-export([load_graph/1, random_operations/3, graph_data_from_file/1]).

add_vertex(VertexData) ->
    [Zone, Name] = string:split(maps:get(<<"id">>, VertexData), ":"),
    ok = client:add_vertex(Zone, maps:get(<<"type">>, VertexData), Name),
    <<Zone/binary, ":", Name/binary>>.

add_edge(EdgeData) ->
    From = maps:get(<<"src">>, EdgeData),
    To = maps:get(<<"dst">>, EdgeData),
    ok = client:add_edge(From, To, maps:get(<<"perms">>, EdgeData), <<"trace">>),
    [From, To].

load_graph(Filename) ->
    {ok, Data} = file:read_file(Filename),
    GraphMap = jiffy:decode(Data, [return_maps]),
    Vertices = maps:get(<<"vertices">>, GraphMap),
    VerticesList = lists:map(fun (V) -> add_vertex(V) end, Vertices),
    Edges = maps:get(<<"edges">>, GraphMap),
    EdgesList = lists:map(fun (E) -> add_edge(E) end, Edges),
    [VerticesList, EdgesList].

graph_data_from_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    GraphMap = jiffy:decode(Data, [return_maps]),
    Vertices = maps:get(<<"vertices">>, GraphMap),
    VerticesList = lists:map(fun (V) -> maps:get(<<"id">>, V) end, Vertices),
    Edges = maps:get(<<"edges">>, GraphMap),
    EdgesList = lists:map(fun (V) -> [maps:get(<<"src">>, V), maps:get(<<"dst">>, V)] end, Edges),
    [VerticesList, EdgesList].

random_members(Vertices) ->
    Index = rand:uniform(length(Vertices)),
    Vertex = lists:nth(Index, Vertices),
    {ok, #{<<"members">> := NaiveResponse}} = client:naive_members(Vertex),
    {ok, #{<<"members">> := IndexedResponse}} = client:indexed_members(Vertex),
    ?assert(client_utils:lists_equal(NaiveResponse, IndexedResponse)).

random_reaches(Vertices) ->
    Index1 = rand:uniform(length(Vertices)),
    Vertex1 = lists:nth(Index1, Vertices),
    Index2 = rand:uniform(length(Vertices)),
    Vertex2 = lists:nth(Index2, Vertices),
    {ok, #{<<"reaches">> := NaiveResponse}} = client:naive_reaches(Vertex1, Vertex2),
    {ok, #{<<"reaches">> := IndexedResponse}} = client:indexed_reaches(Vertex1, Vertex2),
    ?assertEqual(NaiveResponse, IndexedResponse).

random_ep(Vertices) ->
    Index1 = rand:uniform(length(Vertices)),
    Vertex1 = lists:nth(Index1, Vertices),
    Index2 = rand:uniform(length(Vertices)),
    Vertex2 = lists:nth(Index2, Vertices),
    {ok, #{<<"effectivePermissions">> := NaiveResponse}} = client:naive_effective_permissions(Vertex1, Vertex2),
    {ok, #{<<"effectivePermissions">> := IndexedResponse}} = client:indexed_effective_permissions(Vertex1, Vertex2),
    ?assertEqual(NaiveResponse, IndexedResponse).

random_operations(_Type, _Vertices, 0) ->
    ok;
random_operations(members, Vertices, N) ->
    random_members(Vertices),
    random_operations(members, Vertices, N-1);
random_operations(reaches, Vertices, N) ->
    random_reaches(Vertices),
    random_operations(reaches, Vertices, N-1);
random_operations(ep, Vertices, N) ->
    random_ep(Vertices),
    random_operations(ep, Vertices, N-1).