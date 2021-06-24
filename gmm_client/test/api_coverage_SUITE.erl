-module(api_coverage_SUITE).
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
    timer:sleep(1000), %% from unknown reasons some delay is needed before executing first tests - without it tests fail with reason {badmatch, {error, closed}}
    {ok, _} = application:ensure_all_started(gmm_client),
    ok;
init_per_group(_Group, _Config) ->
    ok.

end_per_group(_Group, _Config) ->
    ok.

vertices_adding_test(_Config) ->
    % when
    client:add_user("User1"),
    client:add_user("User2"),
    client:add_user("User3"),
    client:add_user("User4"),

    client:add_group("Group1"),
    client:add_group("Group2"),

    client:add_space("Space1"),

    client:add_provider("Provider1"),
    client:add_provider("Provider2"),
    client:add_provider("Provider3"),

    ExpectedVerticesAndTypes = #{
        "User1" => "user",
        "User2" => "user",
        "User3" => "user",
        "User4" => "user",
        "Group1" => "group",
        "Group2" => "group",
        "Space1" => "space",
        "Provider1" => "provider",
        "Provider2" => "provider",
        "Provider3" => "provider"
    },

    VerticesIds = client_test_helper:get_ids_list(),

    % then
    [
        ?assertEqual(length(maps:keys(ExpectedVerticesAndTypes)), length(VerticesIds)),
        ?assert(client_test_helper:check_vertices_and_types(ExpectedVerticesAndTypes, VerticesIds))
    ].

vertices_deleting_test(_Config) ->
    % when
    client:delete_vertex("zone1/User2"),
    client:delete_vertex("zone1/User3"),
    client:delete_vertex("zone1/Group1"),
    client:delete_vertex("zone1/Provider3"),

    ExpectedVerticesAndTypes = #{
        "User1" => "user",
        "User4" => "user",
        "Group2" => "group",
        "Space1" => "space",
        "Provider1" => "provider",
        "Provider2" => "provider"
    },

    VerticesIds = client_test_helper:get_ids_list(),

    % then
    [
        ?assertEqual(length(maps:keys(ExpectedVerticesAndTypes)), length(VerticesIds)),
        ?assert(client_test_helper:check_vertices_and_types(ExpectedVerticesAndTypes, VerticesIds))
    ].

edges_adding_test(_Config) ->
    % when
    client:add_edge("zone1/User1", "zone1/User4", "01110", "trace1", false),
    client:add_edge("zone1/User1", "zone1/Group2", "10101", "trace2", true),
    client:add_edge("zone1/Group2", "zone1/Provider1", "10001", "trace3", false),
    % then
    [
        ?assertEqual("true", client_test_helper:check_edge_existance("zone1/User1", "zone1/User4")),
        ?assertEqual("\"01110\"", client_test_helper:check_permissions("zone1/User1", "zone1/User4")),

        ?assertEqual("true", client_test_helper:check_edge_existance("zone1/User1", "zone1/Group2")),
        ?assertEqual("\"10101\"", client_test_helper:check_permissions("zone1/User1", "zone1/Group2")),

        ?assertEqual("true", client_test_helper:check_edge_existance("zone1/Group2", "zone1/Provider1")),
        ?assertEqual("\"10001\"", client_test_helper:check_permissions("zone1/Group2", "zone1/Provider1"))
    ].

edges_updating_test(_Config) ->
    ok.

edges_deleting_test(_Config) ->
    % when
    client:delete_edge("zone1/User1", "zone1/User4", "trace1", false),
    client:delete_edge("zone1/Group2", "zone1/Provider1", "trace3", false),
    % then
    [
        ?assertEqual("false", client_test_helper:check_edge_existance("zone1/User1", "zone1/User4")),
        ?assertEqual("true", client_test_helper:check_edge_existance("zone1/User1", "zone1/Group2")),
        ?assertEqual("false", client_test_helper:check_edge_existance("zone1/Group2", "zone1/Provider1"))
    ].
