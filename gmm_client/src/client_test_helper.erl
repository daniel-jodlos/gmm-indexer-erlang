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

%%-export([
%%    get_id_from_list/1,
%%    get_ids_list/0,
%%    get_vertex_name/1,
%%    get_vertex_type/1,
%%    check_edge_existance/2,
%%    check_permissions/2,
%%    check_vertices_and_types/2,
%%    parents_list/1,
%%    children_list/1
%%]).
%%
%%% CONST
%%-define(URL, "localhost:8080/").
%%
%%get_name(List)->
%%    [Head | Tail] = List,
%%
%%    case "name" -- Head of
%%        [] -> Head;
%%        _ -> get_name(Tail)
%%    end.
%%
%%get_type(List)->
%%    [Head | Tail] = List,
%%
%%    case "type" -- Head of
%%        [] -> Head;
%%        _ -> get_type(Tail)
%%    end.
%%
%%is_id(Elem) ->
%%    case "zone" -- Elem of
%%        [] -> true;
%%        _ -> false
%%    end.
%%
%%get_ids_list() ->
%%    application:ensure_all_started(hackney),
%%    {ok, Body} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
%%    lists:filter(fun (Elem) -> is_id(Elem) end, string:tokens(binary:bin_to_list(Body),"\"")).
%%
%%get_id_from_list(N)->
%%    lists:nth(N, get_ids_list()).
%%
%%get_vertex_name(Id)->
%%    application:ensure_all_started(hackney),
%%    Url= ?URL++"graph/vertices/details?id="++Id,
%%    {ok, Body} = client_requests:get_delete_request(Url, get),
%%    NamePart = get_name(string:tokens(binary:bin_to_list(Body),",")),
%%    [_ | Tail] = string:tokens(NamePart, ":"),
%%    [Head | _] = Tail,
%%    Head -- "\"\"\\\\".
%%
%%get_vertex_type(Id)->
%%    application:ensure_all_started(hackney),
%%    Url= ?URL++"graph/vertices/details?id="++Id,
%%    {ok, Body} = client_requests:get_delete_request(Url, get),
%%    TypePart = get_type(string:tokens(binary:bin_to_list(Body),",")),
%%    [_ | Tail] = string:tokens(TypePart, ":"),
%%    [Head | _] = Tail,
%%    Head -- "\"\"\\\\".
%%
%%check_vertices_and_types(ExpectedVerticesAndTypes, VerticesIds) ->
%%    lists:all(fun (Id) -> get_vertex_type(Id) == maps:get(get_vertex_name(Id), ExpectedVerticesAndTypes) end, VerticesIds).
%%
%%check_existance_result(Result) ->
%%    [Head | Tail] = Result,
%%    {Key, Value} = Head,
%%    case "location" -- binary:bin_to_list(Key) of
%%        [] -> binary:bin_to_list(Value);
%%        _ -> check_existance_result(Tail)
%%    end.
%%
%%check_edge_existance(From, To)->
%%    {ok, _, Body, _} = client:check_edge_existance(From, To),
%%    check_existance_result(Body).
%%
%%check_permission_result(Result)->
%%    [Head | Tail] = Result,
%%    {Key, Value} = Head,
%%    case "location" -- binary:bin_to_list(Key) of
%%        [] -> binary:bin_to_list(Value);
%%        _ -> check_existance_result(Tail)
%%    end.
%%
%%check_permissions(From, To)->
%%    {ok, _, Body, _} = client:get_edge_permissions(From, To),
%%    check_permission_result(Body).
%%
%%get_list_from_string(ListString) ->
%%    string:tokens(string:slice(ListString, 1, length(ListString)-2), ",").
%%
%%get_list_from_body(Body)->
%%    [Head | Tail] = Body,
%%    {Key, Value} = Head,
%%    case "location" -- binary:bin_to_list(Key) of
%%        [] -> get_list_from_string(binary:bin_to_list(Value));
%%        _ -> get_list_from_body(Tail)
%%    end.
%%
%%parents_list(Of) ->
%%    {ok, _, Body, _} = client:get_vertex_parents(Of),
%%    get_list_from_body(Body).
%%
%%children_list(Of) ->
%%    {ok, _, Body, _} = client:get_vertex_children(Of),
%%    get_list_from_body(Body).
