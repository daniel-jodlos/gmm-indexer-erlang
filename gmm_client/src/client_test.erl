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
    client:add_user("Jan"),
    client:add_group("Friends"),
    client:add_space("Space"),
    client:add_provider("Provider"),
    UserId = client_test_helper:get_id_from_the_list(),
    [
      ?assertEqual("Jan", client_test_helper:get_vertex_name(UserId))
    ],
    client:delete_vertex(UserId),
    SpaceId = client_test_helper:get_id_from_the_list(),
    [
      ?assertEqual("Space", client_test_helper:get_vertex_name(SpaceId))
    ],
    client:delete_vertex(SpaceId).


edges_test()->
    [
        ]
    .