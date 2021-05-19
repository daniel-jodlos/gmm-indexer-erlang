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
        {<<"application/json">>, from_json}
    ], Req, State}.

resource_exists(Req, _State) ->
    Method = cowboy_req:method(Req),
    #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
%%    io:format("Method: ~p, Id: ~p\n", [Method, Id]),
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
    graph:delete(Id),
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
%%    io:format("DataRaw: ~p\n", [DataRaw]),
    Parsed = case json_utils:decode(DataRaw) of
                 #{<<"type">> := Type, <<"name">> := Name} when
                     Type =:= <<"user">>; Type =:= <<"group">> -> {Type, Name};
                 _ -> undefined
             end,
%%    io:format("Parsed: ~p\n", [Parsed]),
    {handle_post(Parsed), Req, State}.

% inner handler

handle_post(undefined) ->
    false;

handle_post({<<"user">>, Name}) ->
%%    io:format("Adding user...\n", []),
%%    io:format("Result: ~p\n", [graph:add_user(Name)]),
    {ok, Id} = graph:add_user(Name),
%%    io:format("User added\n", []),
    {true, json_utils:encode(#{<<"id">> => Id})};
%%    {true, json_utils:encode(#{<<"id">> => 0})};

%% @todo implement adding groups -> graph:add_group(Name)
handle_post({<<"group">>, Name}) ->
    {ok, Id} = graph:add_user(Name),
    {true, json_utils:encode(#{<<"id">> => Id})}.


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
    {ok, NodesIds} = graph:get_vertices(),
    json_utils:encode(#{<<"nodes">> => NodesIds});

%% @todo implement getting info about node, not only about user
handle_get(Id) ->
    case graph:get_vertex(Id) of
        {ok, undefined} -> json_utils:empty_json();
        {ok, Result} -> Result
    end.
