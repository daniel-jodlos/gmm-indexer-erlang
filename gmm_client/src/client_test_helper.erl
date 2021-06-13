%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. maj 2021 12:54
%%%-------------------------------------------------------------------
-module(client_test_helper).
-author("Piotr Świderski").

-export([get_id_from_the_list/0, get_vertex_name/1, get_second_id_from_the_list/0, check_existance/2, check_permissions/2]).

% CONST
-define(URL, "localhost:8080/").

get_id(List)->
  [Head | Tail] = List,
    
  case "zone" -- Head of
    [] -> Head;
    _ -> get_id(Tail)
  end.


get_second_id(List)->
  [Head | Tail] = List,
    
  case "zone" -- Head of
    [] -> get_id(List);
    _ -> get_second_id(Tail)
  end.

get_name(List)->
  [Head | Tail] = List,
    
  case "name" -- Head of
    [] -> Head;
    _ -> get_name(Tail)
  end.

get_id_from_the_list()->
  application:ensure_all_started(hackney),
  {ok, Body} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
  get_id(string:tokens(binary:bin_to_list(Body),"\"")).

get_second_id_from_the_list()->
  application:ensure_all_started(hackney),
  {ok, Body} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
  get_second_id(string:tokens(binary:bin_to_list(Body),"\"")).

get_vertex_name(Id)->
  application:ensure_all_started(hackney),
  Url= ?URL++"graph/vertices/details?id="++Id,
  {ok, Body} = client_requests:get_delete_request(Url, get),
  NamePart = get_name(string:tokens(binary:bin_to_list(Body),",")), 
  [_ | Tail] = string:tokens(NamePart, ":"),
  [Head | _] = Tail,
  Head -- "\"\"\\\\".

check_existance_result(Result) ->
  [Head | Tail] = Result,
  {Key, Value} = Head,
  case "location" -- binary:bin_to_list(Key) of
    [] -> binary:bin_to_list(Value);
    _ -> check_existance_result(Tail)
  end.

check_existance(From, To)->
  {ok, _, Body, _} = client:check_edge_existance(From, To),
  check_existance_result(Body).

check_permission_result(Result)->
  [Head | Tail] = Result,
  {Key, Value} = Head,
  case "location" -- binary:bin_to_list(Key) of
    [] -> binary:bin_to_list(Value);
    _ -> check_existance_result(Tail)
  end.

check_permissions(From, To)->
  {ok, _, Body, _} = client:get_edge_permissions(From, To),
  check_permission_result(Body).
