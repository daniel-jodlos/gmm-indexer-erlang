%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(client).
-author("Piotr Świderski").


%% API
-export([add_user/1, add_group/1, add_space/1, add_provider/1, 
  get_vertices_list/0, get_edges_list/0, get_vertex_info/1, delete_vertex/1,
  add_edge/3, get_edge_info/2, get_vertex_neighbours/2 ,delete_edge/2]).

% CONST
-define(URL, "localhost:8080/").

% Get Id List Section
list_body_elements(Body) ->
  Format = fun(Element) -> io:format("~s~n", [Element]) end,
  lists:foreach(Format, string:split(Body, ",")).

% VERTICES

add_user(Name)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices?type=user&name="++Name,
  client_requests:post_request(list_to_binary(Url)).

add_group(Name)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices?type=group&name="++Name,
  client_requests:post_request(list_to_binary(Url)).

add_space(Name)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices?type=space&name="++Name,
  client_requests:post_request(list_to_binary(Url)).

add_provider(Name)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices?type=provider&name="++Name,
  client_requests:post_request(list_to_binary(Url)).

get_vertices_list()->
  application:ensure_all_started(hackney),
  {ok, Body} = client_requests:get_simple_request_body(?URL++"graph/vertices"),
  list_body_elements(binary:bin_to_list(Body)).

% do ewentualnej serializacji
get_vertex_info(Id)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices?id="++Id,
  {ok, Body} = client_requests:get_delete_request(Url, get),
  list_body_elements(binary:bin_to_list(Body)).


delete_vertex(Id)->
  application:ensure_all_started(hackney),
  Url = Url= ?URL++"graph/vertices?id="++Id,
  client_requests:get_delete_request(Url, delete).

% EDGES

add_edge(Parent, Child, Permissions)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges?parent="++Parent++"&child="++Child++"&permissions="++Permissions,
  client_requests:post_request(list_to_binary(Url)).

% do ewentualnej serializacji
get_vertex_neighbours(Vertex, WhichEgdes)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges?vertex="++Vertex++"&which_edges="++WhichEgdes,
  {ok, Body} = client_requests:get_delete_request(Url, get),
  list_body_elements(binary:bin_to_list(Body)).

get_edge_info(Parent, Child)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges?parent="++Parent++"&child="++Child,
  {ok, Body} = client_requests:get_delete_request(Url, get),
  list_body_elements(binary:bin_to_list(Body)).

delete_edge(Parent, Child)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges?parent="++Parent++"&child="++Child,
  client_requests:get_delete_request(Url, delete).