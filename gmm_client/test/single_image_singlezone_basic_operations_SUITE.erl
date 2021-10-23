-module(single_image_singlezone_basic_operations_SUITE).
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
    [
        {ct_hooks, [docker_compose_cth]},
        {docker_compose_file, "../../single_image.docker-compose.yml"}
        | Config
    ].

end_per_suite(_Config) ->
    ok = application:stop(gmm_client).

init_per_group(coverage, _Config) ->
    timer:sleep(5000), %% for unknown reasons some delay is needed before executing first tests -
                       %% without it tests fail with reason {badmatch, {error, closed}}
                       %% <<PAWEL>>: It seems that server is not ready immediately; {error, close} indicates,
                       %%            that hackney received error from tcp port
    {ok, _} = application:ensure_all_started(gmm_client),
    ok;
init_per_group(_Group, _Config) ->
    ok.

end_per_group(_Group, _Config) ->
    ok.

vertices_adding_test(_Config) ->
    % given
    Zone = <<"zone0">>,

    % when
    ok = client:add_vertex(Zone, <<"user">>, <<"User1">>),
    ok = client:add_vertex(Zone, <<"user">>, <<"User2">>),
    ok = client:add_vertex(Zone, <<"user">>, <<"User3">>),
    ok = client:add_vertex(Zone, <<"user">>, <<"User4">>),

    ok = client:add_vertex(Zone, <<"group">>, <<"Group1">>),
    ok = client:add_vertex(Zone, <<"group">>, <<"Group2">>),

    ok = client:add_vertex(Zone, <<"space">>, <<"Space1">>),

    ok = client:add_vertex(Zone, <<"provider">>, <<"Provider1">>),
    ok = client:add_vertex(Zone, <<"provider">>, <<"Provider2">>),
    ok = client:add_vertex(Zone, <<"provider">>, <<"Provider3">>),

    {ok, RealVertices} = client:get_all_vertices(Zone),

    ExpectedVertices = #{
        <<"users">> => [<<"zone0:User1">>, <<"zone0:User2">>, <<"zone0:User3">>, <<"zone0:User4">>],
        <<"groups">> => [<<"zone0:Group1">>, <<"zone0:Group2">>],
        <<"spaces">> => [<<"zone0:Space1">>],
        <<"providers">> => [<<"zone0:Provider1">>, <<"zone0:Provider2">>, <<"zone0:Provider3">>]
    },

    % then
    ?assert(client_utils:lists_equal(maps:keys(ExpectedVertices), maps:keys(RealVertices))),
    lists:foreach(
        fun(Type) ->
            ?assert(client_utils:lists_equal(maps:get(Type, ExpectedVertices), maps:get(Type, RealVertices)))
        end,
        maps:keys(ExpectedVertices)
    ).

vertices_deleting_test(_Config) ->
    % when
    ok = client:delete_vertex(<<"zone0:User2">>),
    ok = client:delete_vertex(<<"zone0:User3">>),
    ok = client:delete_vertex(<<"zone0:Group1">>),
    ok = client:delete_vertex(<<"zone0:Provider3">>),

    {ok, RealVertices} = client:get_all_vertices(<<"zone0">>),

    ExpectedVertices = #{
        <<"users">> => [<<"zone0:User1">>, <<"zone0:User4">>],
        <<"groups">> => [<<"zone0:Group2">>],
        <<"spaces">> => [<<"zone0:Space1">>],
        <<"providers">> => [<<"zone0:Provider1">>, <<"zone0:Provider2">>]
    },

    % then
    ?assert(client_utils:lists_equal(maps:keys(ExpectedVertices), maps:keys(RealVertices))),
    lists:foreach(
        fun(Type) ->
            ?assert(client_utils:lists_equal(maps:get(Type, ExpectedVertices), maps:get(Type, RealVertices)))
        end,
        maps:keys(ExpectedVertices)
    ).

edges_adding_test(_Config) ->
    % when
    ok = client:add_edge(<<"zone0:User1">>, <<"zone0:Space1">>, <<"01110">>, <<"trace1">>),
    ok = client:add_edge(<<"zone0:User1">>, <<"zone0:Group2">>, <<"10101">>, <<"trace2">>),
    ok = client:add_edge(<<"zone0:Group2">>, <<"zone0:Provider1">>, <<"10001">>, <<"trace3">>),

    {ok, User1Parents} = client:list_parents(<<"zone0:User1">>),
    {ok, User1Children} = client:list_children(<<"zone0:User1">>),

    {ok, Space1Parents} = client:list_parents(<<"zone0:Space1">>),
    {ok, Space1Children} = client:list_children(<<"zone0:Space1">>),

    {ok, Group2Parents} = client:list_parents(<<"zone0:Group2">>),
    {ok, Group2Children} = client:list_children(<<"zone0:Group2">>),

    {ok, Provider1Parents} = client:list_parents(<<"zone0:Provider1">>),
    {ok, Provider1Children} = client:list_children(<<"zone0:Provider1">>),

    % then
    ?assertEqual({ok, true}, client:edge_exists(<<"zone0:User1">>, <<"zone0:Space1">>)),
    ?assertEqual({ok, <<"01110">>}, client:get_permissions(<<"zone0:User1">>, <<"zone0:Space1">>)),

    ?assertEqual({ok, true}, client:edge_exists(<<"zone0:User1">>, <<"zone0:Group2">>)),
    ?assertEqual({ok, <<"10101">>}, client:get_permissions(<<"zone0:User1">>, <<"zone0:Group2">>)),

    ?assertEqual({ok, true}, client:edge_exists(<<"zone0:Group2">>, <<"zone0:Provider1">>)),
    ?assertEqual({ok, <<"10001">>}, client:get_permissions(<<"zone0:Group2">>, <<"zone0:Provider1">>)),

    ?assertEqual(2, length(User1Parents)),
    ?assertEqual(0, length(User1Children)),

    ?assertEqual(0, length(Space1Parents)),
    ?assertEqual(1, length(Space1Children)),

    ?assertEqual(1, length(Group2Parents)),
    ?assertEqual(1, length(Group2Children)),

    ?assertEqual(0, length(Provider1Parents)),
    ?assertEqual(1, length(Provider1Children)),

    ?assert(lists:member(<<"zone0:Group2">>, User1Parents)),
    ?assert(lists:member(<<"zone0:Space1">>, User1Parents)),
    ?assert(lists:member(<<"zone0:Provider1">>, Group2Parents)),

    ?assert(lists:member(<<"zone0:User1">>, Space1Children)),
    ?assert(lists:member(<<"zone0:User1">>, Group2Children)),
    ?assert(lists:member(<<"zone0:Group2">>, Provider1Children)).

edges_updating_test(_Config) ->
    % when
    ok = client:set_permissions(<<"zone0:User1">>, <<"zone0:Group2">>, <<"00010">>, <<"trace2">>),
    ok = client:set_permissions(<<"zone0:Group2">>, <<"zone0:Provider1">>, <<"11111">>, <<"trace3">>),

    % then
    ?assertEqual({ok, <<"01110">>}, client:get_permissions(<<"zone0:User1">>, <<"zone0:Space1">>)),
    ?assertEqual({ok, <<"00010">>}, client:get_permissions(<<"zone0:User1">>, <<"zone0:Group2">>)),
    ?assertEqual({ok, <<"11111">>}, client:get_permissions(<<"zone0:Group2">>, <<"zone0:Provider1">>)).

edges_deleting_test(_Config) ->
    % when
    ok = client:delete_edge(<<"zone0:User1">>, <<"zone0:Space1">>, <<"trace1">>),
    ok = client:delete_edge(<<"zone0:Group2">>, <<"zone0:Provider1">>, <<"trace3">>),

    {ok, User1Parents} = client:list_parents(<<"zone0:User1">>),
    {ok, User1Children} = client:list_children(<<"zone0:User1">>),

    {ok, Space1Parents} = client:list_parents(<<"zone0:Space1">>),
    {ok, Space1Children} = client:list_children(<<"zone0:Space1">>),

    {ok, Group2Parents} = client:list_parents(<<"zone0:Group2">>),
    {ok, Group2Children} = client:list_children(<<"zone0:Group2">>),

    {ok, Provider1Parents} = client:list_parents(<<"zone0:Provider1">>),
    {ok, Provider1Children} = client:list_children(<<"zone0:Provider1">>),

    % then
    ?assertEqual({ok, false}, client:edge_exists(<<"zone0:User1">>, <<"zone0:Space1">>)),
    ?assertEqual({ok, true}, client:edge_exists(<<"zone0:User1">>, <<"zone0:Group2">>)),
    ?assertEqual({ok, false}, client:edge_exists(<<"zone0:Group2">>, <<"zone0:Provider1">>)),

    ?assertEqual(1, length(User1Parents)),
    ?assertEqual(0, length(User1Children)),

    ?assertEqual(0, length(Space1Parents)),
    ?assertEqual(0, length(Space1Children)),

    ?assertEqual(0, length(Group2Parents)),
    ?assertEqual(1, length(Group2Children)),

    ?assertEqual(0, length(Provider1Parents)),
    ?assertEqual(0, length(Provider1Children)),

    ?assert(lists:member(<<"zone0:Group2">>, User1Parents)),
    ?assertNot(lists:member(<<"zone0:Space1">>, User1Parents)),
    ?assertNot(lists:member(<<"zone0:Provider1">>, Group2Parents)),

    ?assertNot(lists:member(<<"zone0:User1">>, Space1Children)),
    ?assert(lists:member(<<"zone0:User1">>, Group2Children)),
    ?assertNot(lists:member(<<"zone0:Group2">>, Provider1Children)).
