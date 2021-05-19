%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2021 11:39
%%%-------------------------------------------------------------------
-module(gmm_rest_handler_edges).
-author("pawel").

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

-export([
    from_json/2,
    to_json/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, _State) ->
    Method = cowboy_req:method(Req),
    ParametersMap = case Method of
                        <<"GET">> ->
                            case cowboy_req:match_qs([parent, child, vertex, which_edges], Req) of
                                Map when Map =:= #{parent := _, child := _}; Map =:= #{vertex := _, which_edges := _};
                                             Map =:= #{vertex := _} -> maps:put(method, Method, Map)
                            end;
                        <<"POST">> ->
                            maps:put(method, Method, cowboy_req:match_qs([
                                {parent, nonempty}, {child, nonempty}, {permissions, nonempty}], Req));
                        <<"DELETE">> ->
                            maps:put(method, Method, cowboy_req:match_qs([{parent, nonempty}, {child, nonempty}], Req))
                    end,
    {cowboy_rest, Req, ParametersMap}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/xml">>, from_json},
        {<<"application/json">>, from_json}
    ], Req, State}.

resource_exists(Req, State) ->
    #{method := Method} = State,
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
    #{parent := Parent, child := Child} = State,
    case graph:remove_edge(Parent, Child) of
        ok -> {true, Req, State};
        _ -> {false, Req, State}
    end.

delete_completed(Req, State) ->
    {false, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% POST handler

from_json(Req, State) ->
    #{parent := Parent, child := Child, permissions := Permissions} = State,
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
