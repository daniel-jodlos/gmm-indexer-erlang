-module(basic_SUITE).
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
    vertices_test/1,
    edges_test/1
]).

groups() -> [{basic_flow, [sequence], [
    vertices_test,
    edges_test
]}].

all() -> [{group, basic_flow}].

init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

end_per_suite(_Config) ->
    ok = application:stop(gmm_client).

init_per_group(basic_flow, _Config) ->
    timer:sleep(1000), %% from unknown reasons some delay is needed before executing first tests - without it tests fail with reason {badmatch, {error, closed}}
    {ok, _} = application:ensure_all_started(gmm_client),
    ok;
init_per_group(_Group, _Config) ->
    ok.

end_per_group(_Group, _Config) ->
    ok.

vertices_test(_Config)->
    % given
    client:add_user("Jan"),
    client:add_group("Friends"),
    client:add_space("Space"),
    client:add_provider("Provider"),
    % when
    UserId = client_test_helper:get_id_from_list(1),

    % then
    [
      ?assertEqual("Jan", client_test_helper:get_vertex_name(UserId))
    ],
    % when
    client:delete_vertex(UserId),
    SpaceId = client_test_helper:get_id_from_list(1),
    IdsList = client_test_helper:get_ids_list(),

    % then
    [
      ?assertEqual("Space", client_test_helper:get_vertex_name(SpaceId)),
      ?assertNot(lists:member(UserId, IdsList))
    ],
    client:delete_vertex(SpaceId).


edges_test(_Config)->
  % given
  client:add_user("Jan"),
  client:add_user("Filip"),

  JanId = client_test_helper:get_id_from_list(1),
  FilipId = client_test_helper:get_id_from_list(2),

  Permissions = "01110",
  Trace = "trace",
  Successive = true,

  NewPermissions = "00011",

  % when
  client:add_edge(JanId, FilipId, Permissions, Trace, Successive),

  % then
    [
      ?assertEqual("true", client_test_helper:check_edge_existance(JanId, FilipId)),
      ?assertEqual("\""++Permissions++"\"", client_test_helper:check_permissions(JanId, FilipId))
    ],

    % when
    client:set_edge_permissions(JanId, FilipId, NewPermissions, Trace, Successive),

    % then
    [
      ?assertEqual("\""++NewPermissions++"\"", client_test_helper:check_permissions(JanId, FilipId))
    ],
    % when
    client:delete_edge(JanId, FilipId, Trace, Successive),

    % then 
    [
      ?assertEqual("false", client_test_helper:check_edge_existance(JanId, FilipId))
    ],
    client:delete_vertex(JanId),
    client:delete_vertex(FilipId).