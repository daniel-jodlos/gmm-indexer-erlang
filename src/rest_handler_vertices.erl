%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2021 21:12
%%%-------------------------------------------------------------------
-module(rest_handler_vertices).
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
                                             cowboy_req:match_qs([{id, [], undefined}], Req);
                                         <<"POST">> ->
                                             cowboy_req:match_qs([{type, nonempty}, {name, nonempty}], Req);
                                         <<"DELETE">> ->
                                             cowboy_req:match_qs([{id, nonempty}], Req)
                                     end),
    {cowboy_rest, Req, NewState}.

resource_exists(Req, State) ->
    Method = maps:get(method, State),
    Id = case is_map_key(id, State) of
             true -> maps:get(id, State);
             _ -> undefined
         end,
    Result = case {Method, Id} of
                 {<<"GET">>, undefined} -> true;
                 {<<"GET">>, _} -> graph:vertex_exists(Id);
                 {<<"DELETE">>, undefined} -> false;
                 {<<"DELETE">>, _} -> graph:vertex_exists(Id);
                 {<<"POST">>, _} -> false
             end,
    {Result, Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    graph:remove_vertex(maps:get(id, State)),
    {true, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST

% callback
from_json(Req, State) ->
    {handle_post(maps:get(type, State), maps:get(name, State)), Req, State}.

% inner handler
handle_post(Type, Name) when Type =:= <<"user">>; Type =:= <<"group">>; Type =:= <<"space">>; Type =:= <<"provider">> ->
    case graph:create_vertex(Type, Name) of
        {ok, Id} -> {true, json_utils:encode(#{<<"id">> => Id})};
        _ -> false
    end;

handle_post(_, _) ->
    false.

%% GET

% callback
to_json(Req, State) ->
    Id = case maps:get(id, State) of
             undefined -> listing;
             Val -> Val
         end,
    {handle_get(Id), Req, State}.

% inner handler
handle_get(listing) ->
    {ok, VerticesMap} = graph:list_vertices(),
    json_utils:encode(VerticesMap);

handle_get(Id) ->
    {ok, Vertex} = graph:get_vertex(Id),
    json_utils:encode(Vertex).
