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
-export([add_user/1, add_group/1, add_space/1, add_provider/1, check_edge_existance/2,
  get_vertices_list/0, get_vertex_info/1, delete_vertex/1, set_edge_permissions/5, get_vertex_children/1,
  add_edge/5, delete_edge/4, get_edge_permissions/2, get_vertex_parents/1]).

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
  {ok, Body} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
  list_body_elements(binary:bin_to_list(Body)).

% do ewentualnej serializacji
get_vertex_info(Id)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices/details?id="++Id,
  {ok, Body} = client_requests:get_delete_request(Url, get),
  list_body_elements(binary:bin_to_list(Body)).


delete_vertex(Id)->
  application:ensure_all_started(hackney),
  Url = ?URL++"graph/vertices/delete?id="++Id,
  client_requests:post_request(Url).

% EDGES

add_edge(From, To, Permissions, Trace, Successive)->
  application:ensure_all_started(hackney),
  TraceString = case Trace of
                  undefined -> "";
                  _ -> "&trace="++Trace
                end,
  Url= ?URL++"graph/edges?from="++From++"&to="++To++"&permissions="++Permissions++TraceString++"&successive="++Successive,
  client_requests:post_request(list_to_binary(Url)).

% do ewentualnej serializacji
set_edge_permissions(From, To, Permissions, Trace, Successive)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges/permissions?from="++From++"&to="++To++"&permissions="++Permissions++"&trace="++Trace++"&successive="++Successive,
  client_requests:post_request(list_to_binary(Url)).

check_edge_existance(From, To)->
  application:ensure_all_started(hackney),
  Url= ?URL++"is_adjacent?from="++From++"&to="++To,
  client_requests:post_request(list_to_binary(Url)).

get_edge_permissions(From, To)->
  application:ensure_all_started(hackney),
  Url= ?URL++"permissions?from="++From++"&to="++To,
  client_requests:post_request(list_to_binary(Url)).

get_vertex_children(Of)->
  application:ensure_all_started(hackney),
  Url= ?URL++"is_adjacent?of="++Of,
  client_requests:post_request(list_to_binary(Url)).

get_vertex_parents(Of)->
  application:ensure_all_started(hackney),
  Url= ?URL++"is_adjacent_reversed?of="++Of,
  client_requests:post_request(list_to_binary(Url)).
delete_edge(From, To, Trace, Successive)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/edges/delete?from="++From++"&to="++To++"&trace="++Trace++"&successive="++Successive,
  client_requests:post_request(list_to_binary(Url)).
