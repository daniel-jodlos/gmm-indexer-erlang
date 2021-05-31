%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for checking existence and properties of edges,
%%  as well as listing parents or children of given vertex.
%% @end
%%%-------------------------------------------------------------------

-module(rest_basic_queries).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    resource_exists/2
]).

-export([
    from_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    Method = cowboy_req:method(Req),
    ParsedParams =
        case maps:get(operation, State) of
            Op when Op =:= is_adjacent; Op =:= permissions ->
                cowboy_req:match_qs([{from, nonempty}, {to, nonempty}], Req);
            Op when Op =:= list_adjacent; Op =:= list_adjacent_reversed ->
                cowboy_req:match_qs([{'of', nonempty}], Req)
        end,
    NewState = maps:merge(maps:put(method, Method, State), ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    Flag = case State of
               #{from := From, to := To} ->
                   {ok, FromExists} = graph:vertex_exists(From),
                   {ok, ToExists} = graph:vertex_exists(To),
                   VerticesExist = lists:all(fun(X) -> X end, [FromExists, ToExists]),
                   case {VerticesExist, maps:get(operation, State)} of
                       {false, _} -> false;
                       {true, permissions} ->
                           {ok, EdgeExists} = graph:edge_exists(From, To),
                           EdgeExists;
                       {true, _} -> true
                   end;
               #{'of' := Of} ->
                   {ok, OfEx} = graph:vertex_exists(Of),
                   OfEx
           end,
    {Flag, Req, State}.

%% POST handler
from_json(Req, State) ->
    Result = case maps:get(operation, State) of
                 is_adjacent -> graph:edge_exists(maps:get(from, State), maps:get(to, State));
                 list_adjacent -> graph:list_children(maps:get('of', State));
                 list_adjacent_reversed -> graph:list_parents(maps:get('of', State));
                 permissions ->
                     case graph:get_edge(maps:get(from, State), maps:get(to, State)) of
                         {ok, #{<<"Permissions">> := Permissions}} -> {ok, Permissions};
                         _ -> {error, "Didn't obtain edge"}
                     end
             end,
    case Result of
        {ok, Value} -> {{true, gmm_utils:encode(Value)}, Req, State};
        {error, _} -> {false, Req, State}
    end.


%%%---------------------------
%% internal functions
%%%---------------------------

