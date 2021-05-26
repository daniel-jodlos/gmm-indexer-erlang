%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for manipulating edges.
%% @end
%%%-------------------------------------------------------------------

-module(rest_edges).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    allowed_methods/2,
    is_conflict/2
]).

-export([
    from_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    Method = cowboy_req:method(Req),
    ParsedParams = case maps:get(op, State) of
                       Op when Op =:= add; Op =:= permissions ->
                           cowboy_req:match_qs([
                               {from, nonempty}, {to, nonempty}, {permissions, nonempty},
                               {trace, [], undefined}, {successive, nonempty}
                           ], Req);
                       delete ->
                           cowboy_req:match_qs([
                               {from, nonempty}, {to, nonempty}, {trace, [], undefined}, {successive, nonempty}
                           ], Req)
                   end,
    NewState = maps:merge(maps:put(method, Method, State), ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    #{from := From, to := To} = State,
    {ok, Result} = graph:edge_exists(From, To),
    {Result, Req, State}.

is_conflict(Req, State) ->
    Result = case maps:get(op, State) of
                 add -> true;
                 _ -> false
             end,
    {Result, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

%% POST handler

from_json(Req, State) ->
    Result = case maps:get(op, State) of
                 delete ->
                     #{from := From, to := To} = State,
                     case graph:edge_exists(From, To) of
                         {ok, true} -> graph:remove_edge(From, To);
                         _ -> {error, ""}
                     end;
                 permissions ->
                     #{from := From, to := To, permissions := Permissions} = State,
                     graph:update_edge(From, To, Permissions);
                 add ->
                     #{from := From, to := To, permissions := Permissions} = State,
                     graph:create_edge(From, To, Permissions)
             end,
    Flag = case Result of
               ok -> true;
               {error, _} -> false
           end,
    {Flag, Req, State}.
