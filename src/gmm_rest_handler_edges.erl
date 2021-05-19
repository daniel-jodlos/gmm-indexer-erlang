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
%%    init/2,
%%    allowed_methods/2,
%%    content_types_provided/2,
%%    content_types_accepted/2,
%%    resource_exists/2,
%%    delete_resource/2,
%%    delete_completed/2
]).

-export([
%%    from_json/2,
%%    to_json/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%init(Req, State) ->
%%    {cowboy_rest, Req, State}.
%%
%%allowed_methods(Req, State) ->
%%    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
%%    {Methods, Req, State}.
%%
%%content_types_provided(Req, State) ->
%%    {[
%%        {<<"application/json">>, to_json}
%%    ], Req, State}.
%%
%%content_types_accepted(Req, State) ->
%%    {[
%%        {<<"application/json">>, from_json}
%%    ], Req, State}.
%%
%%resource_exists(Req, State) ->
%%    Id = parse_id_binding(Req),
%%    WhichEdges = parse_which_edges_binding(Req),
%%    case lists:any(fun(Bind) -> Bind =:= error end, [Id, WhichEdges]) of
%%        true -> {false, Req, State};
%%        _ -> {id_exists(Id), Req, State}
%%    end.
%%
%%%% DELETE callback
%%%% @todo implement removing an edge
%%delete_resource(Req, State) ->
%%    Id = cowboy_req:binding(id, Req),
%%    graph:delete_user(Id),
%%    {true, Req, State}.
%%
%%delete_completed(Req, State) ->
%%    {false, Req, State}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%parse_id_binding(Req) ->
%%    case cowboy_req:binding(id, Req) of
%%        undefined -> error;
%%        Id -> Id
%%    end.
%%
%%parse_which_edges_binding(Req) ->
%%    case cowboy_req:binding(which_edges, Req) of
%%        undefined -> all;
%%        <<"parents">> -> parents;
%%        <<"children">> -> children;
%%        _ -> error
%%    end.
%%
%%% inner function of resource_exists callback
%%id_exists(Id) ->
%%    case graph:get_user(Id) of
%%        {ok, undefined} -> false;
%%        {ok, _Result} -> true
%%    end.
%%
%%%% POST handler
%%
%%%% @todo add permissions
%%from_json(Req0, State) ->
%%    {ok, DataRaw, Req} = cowboy_req:read_body(Req0),
%%    case json_utils:decode(DataRaw) of
%%        %% @todo
%%%%        #{<<"parent">> := Parent, <<"child">> := Child, <<"permissions">> := Permissions} ->
%%        #{<<"parent">> := Parent, <<"child">> := Child} ->
%%            {add_edge(Parent, Child, nil), Req, State};
%%        _ -> {false, Req, State}
%%    end.
%%
%%%% @todo refactor this to create double-oriented coupling between nodes
%%add_edge(Parent, Child, _Permissions) ->
%%    case graph:add_user_to_group/2(Child, Parent) of
%%        {error, Reason} -> false;
%%        ok -> true
%%    end.
%%
%%%% GET handler
%%
%%to_json(Req, State) ->
%%    Id = parse_id_binding(Req),
%%    WhichEdges = parse_which_edges_binding(Req),
%%    {handle_get(Id, WhichEdges), Req, State}.
%%
%%% get response about edges of given node
%%%% @todo implement getting parents in graph module
%%handle_get(Id, all) ->
%%    ListChildren = graph:list_group_users(Id),
%%    %% @todo
%%    % ListParents = graph:list_node_parents(Id)
%%    json_utils:encode(#{<<"parents">> => [], <<"children">> => ListChildren});
%%
%%handle_get(_Id, parents) ->
%%    %% @todo
%%    % ListParents = graph_list_node_parents(Id)
%%    json_utils:encode(#{<<"parents">> => []});
%%
%%handle_get(Id, children) ->
%%    ListChildren = graph:list_group_users(Id),
%%    json_utils:encode(#{<<"children">> => ListChildren}).
