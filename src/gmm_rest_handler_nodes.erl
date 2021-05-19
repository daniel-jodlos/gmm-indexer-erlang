-module(gmm_rest_handler_nodes).

-behavior(cowboy_handler).

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

init(Req, State) ->
    {cowboy_rest, Req, State}.

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

resource_exists(Req, _State) ->
    Method = cowboy_req:method(Req),
    #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
    Result = case {Method, Id} of
                 {<<"GET">>, undefined} -> true;
                 {<<"GET">>, _} -> id_exists(Id);
                 {<<"DELETE">>, undefined} -> false;
                 {<<"DELETE">>, _} -> id_exists(Id);
                 {<<"POST">>, _} -> false
             end,
    Map = case Id of
              undefined -> #{};
              _Id -> #{id => Id}
          end,
    {Result, Req, Map}.

%% DELETE callback
delete_resource(Req, State) ->
    {ok, Id} = maps:find(id, State),
    graph:remove_vertex(Id),
    {true, Req, State}.

delete_completed(Req, State) ->
    {false, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% inner function of resource_exists callback
id_exists(Id) ->
    case graph:get_vertex(Id) of
        {ok, undefined} -> false;
        {ok, _Result} -> true
    end.


%% POST

% callback
from_json(Req0, State) ->
    {ok, DataRaw, Req} = cowboy_req:read_body(Req0),
    Parsed = case json_utils:decode(DataRaw) of
                 #{<<"type">> := Type, <<"name">> := Name} -> {Type, Name};
                 _ -> undefined
             end,
    {handle_post(Parsed), Req, State}.

% inner handler

handle_post(undefined) ->
    false;

handle_post({Type, Name}) when Type =:= <<"user">>; Type =:= <<"group">>; Type =:= <<"space">>; Type =:= <<"provider">> ->
    {ok, Id} = graph:create_vertex(Type, Name),
    {true, json_utils:encode(#{<<"id">> => Id})};

handle_post(_) ->
    false.


%% GET

% callback
to_json(Req, State) ->
    Id = case State of
             #{id := Val} -> Val;
             _ -> listing
         end,
    {handle_get(Id), Req, State}.

% inner handler

handle_get(listing) ->
    {ok, VerticesMap} = graph:list_vertices(),
    json_utils:encode(VerticesMap);

handle_get(Id) ->
    case graph:get_vertex(Id) of
        {ok, undefined} -> json_utils:empty_json();
        {ok, Result} -> Result
    end.
