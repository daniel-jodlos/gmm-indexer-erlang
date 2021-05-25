%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2021 21:12
%%%-------------------------------------------------------------------
-module(rest_edges).
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
    NewState = maps:merge(TempState, case Method of
                                         <<"GET">> ->
                                             parse_get_parameters([[parent, child], [vertex, which_edges], [vertex]], Req);
                                         <<"POST">> ->
                                             cowboy_req:match_qs([{parent, nonempty},
                                                 {child, nonempty}, {permissions, nonempty}], Req);
                                         <<"DELETE">> ->
                                             cowboy_req:match_qs([{parent, nonempty}, {child, nonempty}], Req)
                                     end),
    {cowboy_rest, Req, NewState}.

resource_exists(Req, State) ->
    Method = maps:get(method, State),
    Result = case Method of
                 <<"GET">> ->
                     case State of
                         #{parent := Parent, child := Child} ->
                             {ok, Bool} = graph:edge_exists(Parent, Child),
                             Bool;
                         #{vertex := Vertex} ->
                             {ok, Bool} = graph:vertex_exists(Vertex),
                             Bool;
                         _ -> false
                     end;
                 <<"POST">> -> false;
                 <<"DELETE">> ->
                     case State of
                         #{parent := Parent, child := Child} ->
                             {ok, Bool} = graph:edge_exists(Parent, Child),
                             Bool;
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_get_parameters([], _Req) ->
    erlang:error("None of the parameters where found");
parse_get_parameters([List | Rest], Req) ->
    Zipped = lists:map(fun(Field) -> {Field, [], undefined} end, List),
    Results = cowboy_req:match_qs(Zipped, Req),
    UndefinedFields = maps:filter(fun(_K, V) -> V =:= undefined end, Results),
    case map_size(UndefinedFields) of
        0 -> Results;
        _ -> parse_get_parameters(Rest, Req)
    end.

%% POST handler

from_json(Req, State) ->
    #{parent := Parent, child := Child, permissions := Permissions} = State,
    Result = case graph:edge_exists(Parent, Child) of
                 {ok, false} -> graph:create_edge(Parent, Child, Permissions);
                 {ok, true} -> graph:update_edge(Parent, Child, Permissions)
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
