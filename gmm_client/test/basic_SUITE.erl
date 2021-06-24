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
    IdsList = client_test_helper:get_ids_list(),

    % then
    [
      ?assert(lists:member("zone1/Jan", IdsList)),
      ?assert(lists:member("zone1/Friends", IdsList)),
      ?assert(lists:member("zone1/Space", IdsList)),
      ?assert(lists:member("zone1/Provider", IdsList))
    ],
    % when
    client:delete_vertex("zone1/Jan"),
    NewIdsList = client_test_helper:get_ids_list(),

    % then
    [
      ?assertNot(lists:member("zone1/Jan", NewIdsList)),
      ?assert(lists:member("zone1/Friends", NewIdsList)),
      ?assert(lists:member("zone1/Space", NewIdsList)),
      ?assert(lists:member("zone1/Provider", NewIdsList))
    ],
    client:delete_vertex("zone1/Space").


edges_test(_Config)->
  % given
  client:add_user("Jan"),
  client:add_group("Group"),

  JanId = client_test_helper:get_id_from_list(1),
  GroupId = client_test_helper:get_id_from_list(2),

  Permissions = "01110",
  Trace = "trace",
  Successive = false,

  NewPermissions = "00011",

  % when
  client:add_edge(JanId, GroupId, Permissions, Trace, Successive),

  % then
    [
      ?assertEqual("true", client_test_helper:check_edge_existance(JanId, GroupId)),
      ?assertEqual("\""++Permissions++"\"", client_test_helper:check_permissions(JanId, GroupId))
    ],

    % when
    client:set_edge_permissions(JanId, GroupId, NewPermissions, Trace, Successive),

    % then
    [
      ?assertEqual("\""++NewPermissions++"\"", client_test_helper:check_permissions(JanId, GroupId))
    ],
    % when
    client:delete_edge(JanId, GroupId, Trace, Successive),

    % then 
    [
      ?assertEqual("false", client_test_helper:check_edge_existance(JanId, GroupId))
    ],
    client:delete_vertex(JanId),
    client:delete_vertex(GroupId).