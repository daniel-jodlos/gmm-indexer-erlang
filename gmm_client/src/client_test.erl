%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(client_test).
-author("Piotr Świderski").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
% users, spaces, providers, group
vertices_test()->
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


edges_test()->
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