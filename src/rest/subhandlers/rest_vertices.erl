%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for manipulating vertices
%% @end
%%%-------------------------------------------------------------------

-module(rest_vertices).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    delete_resource/2,
    resource_exists/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    Method = cowboy_req:method(Req),
    TempState = maps:put(method, Method, State),
    NewState = maps:merge(TempState, case Method of
                                         <<"GET">> ->
                                             cowboy_req:match_qs([{id, [], listing}], Req);
                                         <<"POST">> ->
                                             cowboy_req:match_qs([{type, nonempty}, {name, nonempty}], Req);
                                         <<"DELETE">> ->
                                             cowboy_req:match_qs([{id, nonempty}], Req)
                                     end),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, State) ->
    Method = maps:get(method, State),
    Id = case is_map_key(id, State) of
             true -> maps:get(id, State);
             _ -> undefined
         end,
    Result = case {Method, Id} of
                 {<<"GET">>, listing} -> true;
                 {<<"GET">>, _} ->
                     {ok, Bool} = graph:vertex_exists(Id),
                     Bool;
                 {<<"DELETE">>, undefined} -> false;
                 {<<"DELETE">>, _} ->
                     {ok, Bool} = graph:vertex_exists(Id),
                     Bool;
                 {<<"POST">>, _} -> false
             end,
    {Result, Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    Result = case graph:remove_vertex(maps:get(id, State)) of
                 ok -> true;
                 {error, _} -> false
             end,
    {Result, Req, State}.

%% POST handler
from_json(Req, State) ->
    {handle_post(maps:get(type, State), maps:get(name, State)), Req, State}.

%% GET handler
to_json(Req, State) ->
    #{id := Id} = State,
    {handle_get(Id), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec handle_post(binary(), binary()) -> {true, binary()} | false.
handle_post(Type, Name) when Type =:= <<"user">>; Type =:= <<"group">>; Type =:= <<"space">>; Type =:= <<"provider">> ->
    case graph:create_vertex(Type, Name) of
        {ok, Id} -> {true, json_utils:encode(#{<<"id">> => Id})};
        _ -> false
    end;
handle_post(_, _) ->
    false.

-spec handle_get(listing | binary()) -> binary().
handle_get(listing) ->
    {ok, VerticesMap} = graph:list_vertices(),
    json_utils:encode(VerticesMap);
handle_get(Id) ->
    {ok, Vertex} = graph:get_vertex(Id),
    json_utils:encode(Vertex).
