-module(basic_operations_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).
-export([
    vertices_adding_test/1,
    vertices_deleting_test/1,
    edges_adding_test/1,
    edges_updating_test/1,
    edges_deleting_test/1
]).

groups() -> [{coverage, [sequence], [
    vertices_adding_test,
    vertices_deleting_test,
    edges_adding_test,
    edges_updating_test,
    edges_deleting_test
]}].

all() -> [{group, coverage}].

init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

end_per_suite(_Config) ->
    ok = application:stop(gmm_client).

init_per_group(coverage, _Config) ->
    timer:sleep(5000), %% from unknown reasons some delay is needed before executing first tests - without it tests fail with reason {badmatch, {error, closed}}
    {ok, _} = application:ensure_all_started(gmm_client),
    ok;
init_per_group(_Group, _Config) ->
    ok.

end_per_group(_Group, _Config) ->
    ok.

vertices_adding_test(_Config) ->
    % when
    Zone = <<"zone1">>,

    client:add_vertex(Zone, <<"user">>, <<"User1">>),
    client:add_vertex(Zone, <<"user">>, <<"User2">>),
    client:add_vertex(Zone, <<"user">>, <<"User3">>),
    client:add_vertex(Zone, <<"user">>, <<"User4">>),

    client:add_vertex(Zone, <<"group">>, <<"Group1">>),
    client:add_vertex(Zone, <<"group">>, <<"Group2">>),

    client:add_vertex(Zone, <<"space">>, <<"Space1">>),

    client:add_vertex(Zone, <<"provider">>, <<"Provider1">>),
    client:add_vertex(Zone, <<"provider">>, <<"Provider2">>),
    client:add_vertex(Zone, <<"provider">>, <<"Provider3">>),

%%    ExpectedVerticesAndTypes = #{
%%        <<"User1">> => <<"user">>,
%%        <<"User2">> => <<"user">>,
%%        <<"User3">> => <<"user">>,
%%        <<"User4">> => <<"user">>,
%%        <<"Group1">> => <<"group">>,
%%        <<"Group2">> => <<"group">>,
%%        <<"Space1">> => <<"space">>,
%%        <<"Provider1">> => <<"provider">>,
%%        <<"Provider2">> => <<"provider">>,
%%        <<"Provider3">> => <<"provider">>
%%    },

    ExpectedVertices = #{
        <<"users">> => [<<"zone1/User1">>, <<"zone1/User2">>, <<"zone1/User3">>, <<"zone1/User4">>],
        <<"groups">> => [<<"zone1/Group1">>, <<"zone1/Group2">>],
        <<"spaces">> => [<<"zone1/Space1">>],
        <<"providers">> => [<<"zone1/Provider1">>, <<"zone1/Provider2">>, <<"zone1/Provider3">>]
    },

    VerticesIds = client:get_all_vertices(Zone),

    % then
    [
        lists:foreach(
            fun(Type) ->
                ?assert(client_utils:lists_equal(maps:get(Type, ExpectedVertices), maps:get(Type, VerticesIds)))
            end,
            maps:keys(ExpectedVertices)
        )
%%        ?assertEqual(length(maps:keys(ExpectedVerticesAndTypes)), length(VerticesIds)),
%%        ?assert(client_test_helper:check_vertices_and_types(ExpectedVerticesAndTypes, VerticesIds))
    ].

vertices_deleting_test(_Config) ->
    % when
    Zone = <<"zone1">>,

    client:delete_vertex(<<"zone1/User2">>),
    client:delete_vertex(<<"zone1/User3">>),
    client:delete_vertex(<<"zone1/Group1">>),
    client:delete_vertex(<<"zone1/Provider3">>),

    ExpectedVerticesAndTypes = #{
        "User1" => "user",
        "User4" => "user",
        "Group2" => "group",
        "Space1" => "space",
        "Provider1" => "provider",
        "Provider2" => "provider"
    },

    ExpectedVertices = #{
        <<"users">> => [<<"zone1/User1">>, <<"zone1/User4">>],
        <<"groups">> => [<<"zone1/Group2">>],
        <<"spaces">> => [<<"zone1/Space1">>],
        <<"providers">> => [<<"zone1/Provider1">>, <<"zone1/Provider2">>]
    },

    VerticesIds = client:get_all_vertices(Zone),

    % then
    [
        lists:foreach(
            fun(Type) ->
                ?assert(client_utils:lists_equal(maps:get(Type, ExpectedVertices), maps:get(Type, VerticesIds)))
            end,
            maps:keys(ExpectedVertices)
        )
%%        ?assertEqual(length(maps:keys(ExpectedVerticesAndTypes)), length(VerticesIds)),
%%        ?assert(client_test_helper:check_vertices_and_types(ExpectedVerticesAndTypes, VerticesIds))
    ].

edges_adding_test(_Config) ->
    % when
    client:add_edge(<<"zone1/User1">>, <<"zone1/Space1">>, <<"01110">>, <<"trace1">>),
    client:add_edge(<<"zone1/User1">>, <<"zone1/Group2">>, <<"10101">>, <<"trace2">>),
    client:add_edge(<<"zone1/Group2">>, <<"zone1/Provider1">>, <<"10001">>, <<"trace3">>),

    {ok, User1Parents} = client:list_parents(<<"zone1/User1">>),
    {ok, User1Children} = client:list_children(<<"zone1/User1">>),

    {ok, Space1Parents} = client:list_parents(<<"zone1/Space1">>),
    {ok, Space1Children} = client:list_children(<<"zone1/Space1">>),

    {ok, Group2Parents} = client:list_parents(<<"zone1/Group2">>),
    {ok, Group2Children} = client:list_children(<<"zone1/Group2">>),

    {ok, Provider1Parents} = client:list_parents(<<"zone1/Provider1">>),
    {ok, Provider1Children} = client:list_children(<<"zone1/Provider1">>),

    % then
    [
        ?assertEqual({ok, true}, client:edge_exists(<<"zone1/User1">>, <<"zone1/Space1">>)),
        ?assertEqual({ok, <<"01110">>}, client:get_permissions(<<"zone1/User1">>, <<"zone1/Space1">>)),

        ?assertEqual({ok, true}, client:edge_exists(<<"zone1/User1">>, <<"zone1/Group2">>)),
        ?assertEqual({ok, <<"10101">>}, client:get_permissions(<<"zone1/User1">>, <<"zone1/Group2">>)),

        ?assertEqual({ok, true}, client:edge_exists(<<"zone1/Group2">>, <<"zone1/Provider1">>)),
        ?assertEqual({ok, <<"10001">>}, client:get_permissions(<<"zone1/Group2">>, <<"zone1/Provider1">>)),

        ?assertEqual(2, length(User1Parents)),
        ?assertEqual(0, length(User1Children)),

        ?assertEqual(0, length(Space1Parents)),
        ?assertEqual(1, length(Space1Children)),

        ?assertEqual(1, length(Group2Parents)),
        ?assertEqual(1, length(Group2Children)),

        ?assertEqual(0, length(Provider1Parents)),
        ?assertEqual(1, length(Provider1Children)),

        ?assert(lists:member(<<"zone1/Group2">>, User1Parents)),
        ?assert(lists:member(<<"zone1/Space1">>, User1Parents)),
        ?assert(lists:member(<<"zone1/Provider1">>, Group2Parents)),

        ?assert(lists:member(<<"zone1/User1">>, Space1Children)),
        ?assert(lists:member(<<"zone1/User1">>, Group2Children)),
        ?assert(lists:member(<<"zone1/Group2">>, Provider1Children))
    ].

edges_updating_test(_Config) ->
    % when
    client:set_permissions(<<"zone1/User1">>, <<"zone1/Group2">>, <<"00010">>, <<"trace2">>),
    client:set_permissions(<<"zone1/Group2">>, <<"zone1/Provider1">>, <<"11111">>, <<"trace3">>),

    % then
    [
        ?assertEqual({ok, <<"01110">>}, client:get_permissions(<<"zone1/User1">>, <<"zone1/Space1">>)),
        ?assertEqual({ok, <<"00010">>}, client:get_permissions(<<"zone1/User1">>, <<"zone1/Group2">>)),
        ?assertEqual({ok, <<"11111">>}, client:get_permissions(<<"zone1/Group2">>, <<"zone1/Provider1">>))
    ].

edges_deleting_test(_Config) ->
    % when
    client:delete_edge(<<"zone1/User1">>, <<"zone1/Space1">>, <<"trace1">>),
    client:delete_edge(<<"zone1/Group2">>, <<"zone1/Provider1">>, <<"trace3">>),

    {ok, User1Parents} = client:list_parents(<<"zone1/User1">>),
    {ok, User1Children} = client:list_children(<<"zone1/User1">>),

    {ok, Space1Parents} = client:list_parents(<<"zone1/Space1">>),
    {ok, Space1Children} = client:list_children(<<"zone1/Space1">>),

    {ok, Group2Parents} = client:list_parents(<<"zone1/Group2">>),
    {ok, Group2Children} = client:list_children(<<"zone1/Group2">>),

    {ok, Provider1Parents} = client:list_parents(<<"zone1/Provider1">>),
    {ok, Provider1Children} = client:list_children(<<"zone1/Provider1">>),

    % then
    [
        ?assertEqual({ok, false}, client:edge_exists(<<"zone1/User1">>, <<"zone1/Space1">>)),
        ?assertEqual({ok, true}, client:edge_exists(<<"zone1/User1">>, <<"zone1/Group2">>)),
        ?assertEqual({ok, false}, client:edge_exists(<<"zone1/Group2">>, <<"zone1/Provider1">>)),

        ?assertEqual(1, length(User1Parents)),
        ?assertEqual(0, length(User1Children)),

        ?assertEqual(0, length(Space1Parents)),
        ?assertEqual(0, length(Space1Children)),

        ?assertEqual(0, length(Group2Parents)),
        ?assertEqual(1, length(Group2Children)),

        ?assertEqual(0, length(Provider1Parents)),
        ?assertEqual(0, length(Provider1Children)),

        ?assert(lists:member(<<"zone1/Group2">>, User1Parents)),
        ?assertNot(lists:member(<<"zone1/Space1">>, User1Parents)),
        ?assertNot(lists:member(<<"zone1/Provider1">>, Group2Parents)),

        ?assertNot(lists:member(<<"zone1/User1">>, Space1Children)),
        ?assert(lists:member(<<"zone1/User1">>, Group2Children)),
        ?assertNot(lists:member(<<"zone1/Group2">>, Provider1Children))
    ].
