-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([
    all/0,
    groups/0,
    init_per_suite/1
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

vertices_test(_Config)->
    % given
    client:add_user("Jan"),
    client:add_group("Friends"),
    client:add_space("Space"),
    client:add_provider("Provider"),
    % when
    UserId = client_test_helper:get_id_from_the_list(),

    % then
    [
      ?assertEqual("Jan", client_test_helper:get_vertex_name(UserId))
    ],
    % when
    client:delete_vertex(UserId),
    SpaceId = client_test_helper:get_id_from_the_list(),

    % then
    [
      ?assertEqual("Space", client_test_helper:get_vertex_name(SpaceId))
    ],
    client:delete_vertex(SpaceId).


edges_test(_Config)->
  % given
  client:add_user("Jan"),
  client:add_user("Filip"),

  JanId = client_test_helper:get_id_from_the_list(),
  FilipId = client_test_helper:get_second_id_from_the_list(),

  Permissions = "permissions",
  Trace = "trace",
  Successive = "successive",

  NewPermissions = "new_permissions",

  % when
  client:add_edge(JanId, FilipId, Permissions, Trace, Successive),

  % then
    [
      ?assertEqual("true", client_test_helper:check_existance(JanId, FilipId)),
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
      ?assertEqual("false", client_test_helper:check_existance(JanId, FilipId))
    ],
    client:delete_vertex(JanId),
    client:delete_vertex(FilipId).