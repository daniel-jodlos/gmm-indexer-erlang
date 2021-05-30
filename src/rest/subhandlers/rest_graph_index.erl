%%%-------------------------------------------------------------------
%% @doc
%%  @todo
%%  Implements getting detailed index of vertices in given list
%%  or all vertices if provided list is empty
%% @end
%%%-------------------------------------------------------------------

-module(rest_graph_index).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    resource_exists/2
]).

-export([
    to_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    BinData = cowboy_req:match_qs([vertices], Req),
    ParsedList = case json_utils:decode(BinData) of
                     null -> [];
                     [] -> [];
                     List when is_list(List) ->
                         true = lists:all(fun is_binary/1, List),
                         List
                 end,
    NewState = maps:put(vertices_list, ParsedList, State),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, State) ->
    Flag = case maps:get(vertices_list, State) of
               [] -> true;
               List -> lists:all(fun(X) -> {ok, Bool} = graph:vertex_exists(X), Bool end, List)
           end,
    {Flag, Req, State}.

%% @todo
%% GET handler
to_json(Req, State) ->
    {json_utils:empty_json(), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

