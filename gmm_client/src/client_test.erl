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


edges_test()->
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