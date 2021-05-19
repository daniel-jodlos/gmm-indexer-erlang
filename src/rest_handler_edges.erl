%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2021 21:12
%%%-------------------------------------------------------------------
-module(rest_handler_edges).
-author("pawel").

%% API
-export([
    init/2,
    resource_exists/2,
    delete_resource/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
    Method = cowboy_req:method(Req),
    TempState = maps:put(method, Method, State),
    NewState = case Method of
                        <<"GET">> ->
                            case cowboy_req:match_qs([parent, child, vertex, which_edges], Req) of
                                Map when
                                    is_map_key(parent, Map), is_map_key(child, Map);
                                    is_map_key(vertex, Map), is_map_key(which_edges, Map);
                                    is_map_key(vertex, Map) ->

                                    maps:merge(TempState, Map)
                            end;
                        <<"POST">> ->
                            maps:merge(TempState, cowboy_req:match_qs([
                                {parent, nonempty}, {child, nonempty}, {permissions, nonempty}], Req));
                        <<"DELETE">> ->
                            maps:merge(TempState, cowboy_req:match_qs([
                                {parent, nonempty}, {child, nonempty}], Req))
                    end,
    {cowboy_rest, Req, NewState}.

resource_exists(Req, State) ->
    Method = maps:get(method, State),
    Result = case Method of
                 <<"GET">> ->
                     case State of
                         #{parent := Parent, child := Child} -> graph:edge_exists(Parent, Child);
                         #{vertex := Vertex} -> graph:vertex_exists(Vertex);
                         _ -> false
                     end;
                 <<"POST">> -> false;
                 <<"DELETE">> ->
                     case State of
                         #{parent := Parent, child := Child} -> graph:edge_exists(Parent, Child);
                         _ -> false
                     end
             end,
    {Result, Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    Parent = maps:get(parent, State),
    Child = maps:get(child, State),
%%    #{parent := Parent, child := Child} = State,
    case graph:remove_edge(Parent, Child) of
        ok -> {true, Req, State};
        _ -> {false, Req, State}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST handler

from_json(Req, State) ->
    Parent = maps:get(parent, State),
    Child = maps:get(child, State),
    Permissions = maps:get(child, State),
%%    #{parent := Parent, child := Child, permissions := Permissions} = State,
    Result = case graph:edge_exists(Parent, Child) of
                 false -> graph:create_edge(Parent, Child, Permissions);
                 true -> graph:update_edge(Parent, Child, Permissions)
             end,
    case Result of
        ok -> {true, Req, State};
        _ -> {false, Req, State}
    end.

%% GET handler

to_json(Req, State) ->
    Result = case State of
                 #{parent := Parent, child := Child} -> get_edge_info(Parent, Child);
                 #{vertex := Vertex, which_edges := WhichEdges} -> get_neighbours(Vertex, WhichEdges);
                 #{vertex := Vertex} -> get_neighbours(Vertex, all)
             end,
    {Result, Req, State}.

% get info about an edge
get_edge_info(Parent, Child) ->
    {ok, Result} = graph:get_edge(Parent, Child),
    json_utils:encode(Result).

% get response about edges of given vertex
get_neighbours(Vertex, <<"parents">>) ->
    {ok, Result} = graph:list_parents(Vertex),
    json_utils:encode(Result);

get_neighbours(Vertex, <<"children">>) ->
    {ok, Result} = graph:list_children(Vertex),
    json_utils:encode(Result);

get_neighbours(Vertex, all) ->
    {ok, Parents} = graph:list_parents(Vertex),
    {ok, Children} = graph:list_children(Vertex),
    json_utils:encode(#{<<"parents">> => Parents, <<"children">> => Children}).
