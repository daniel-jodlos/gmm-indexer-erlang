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

-include("records.hrl").

%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    ParsedParams =
        case maps:get(operation, State) of
            Op when Op =:= is_adjacent; Op =:= permissions ->
                Map = cowboy_req:match_qs([{from, nonempty}, {to, nonempty}], Req),
                ok = gmm_utils:validate_vertex_id(maps:get(from, Map)),
                ok = gmm_utils:validate_vertex_id(maps:get(to, Map)),
                Map;
            Op when Op =:= list_adjacent; Op =:= list_adjacent_reversed ->
                Map = cowboy_req:match_qs([{'of', nonempty}], Req),
                ok = gmm_utils:validate_vertex_id(maps:get('of', Map)),
                Map
        end,
    NewState = maps:merge(State, ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    ZoneId = gmm_utils:zone_id(),
    Flag =
        case State of
            #{'of' := Of} ->
                case gmm_utils:owner_of(Of) of
                    ZoneId -> {ok, Exists} = graph:vertex_exists(Of), Exists;
                    _ -> true
                end;
            #{from := From, to := To} ->
                ZoneId = gmm_utils:zone_id(),
                Checker = fun(X) -> {ok, Exists} = graph:vertex_exists(X),
                    (gmm_utils:owner_of(X) =/= ZoneId) or Exists end,
                VerticesExist = lists:all(Checker, [From, To]),
                EdgeCheckNeeded = (maps:get(operation, State) == permissions) and on_this_zone(From, To),
                case EdgeCheckNeeded of
                    true ->
                        {ok, EdgeExists} = graph:edge_exists(From, To),
                        VerticesExist and EdgeExists;
                    false -> VerticesExist
                end
        end,
    NewState = maps:put(resources_exist, Flag, State),
    {false, Req, NewState}.

%% POST handler
from_json(Req, State) ->
    Result =
        case maps:get(resources_exist, State) of
            true ->
                case maps:get(operation, State) of
                    is_adjacent -> is_adjacent(maps:get(from, State), maps:get(to, State));
                    permissions -> permissions(maps:get(from, State), maps:get(to, State));
                    list_adjacent -> list_adjacent(maps:get('of', State));
                    list_adjacent_reversed -> list_adjacent_reversed(maps:get('of', State))
                end;
            false ->
                {error, "Resource not found"}
        end,
    case Result of
        {ok, Value} ->
            Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Value), Req),
            {true, Req1, State};
        {error, _} -> {false, Req, State}
    end.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec on_this_zone(V1 :: binary(), V2 :: binary()) -> boolean().
on_this_zone(V1, V2) ->
    lists:any(fun(X) -> gmm_utils:owner_of(X) == gmm_utils:zone_id() end, [V1, V2]).

-spec is_adjacent(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
is_adjacent(From, To) ->
    case on_this_zone(From, To) of
        true -> graph:edge_exists(From, To);
        false -> zone_client:is_adjacent(gmm_utils:owner_of(From), From, To)
    end.

-spec permissions(From :: binary(), To :: binary()) -> {ok, permissions()} | {error, any()}.
permissions(From, To) ->
    case on_this_zone(From, To) of
        true ->
            case graph:get_edge(From, To) of
                {ok, #{<<"permissions">> := Permissions}} -> {ok, Permissions};
                _ -> {error, "Didn't obtain edge"}
            end;
        false -> zone_client:permissions(gmm_utils:owner_of(From), From, To)
    end.

-spec list_adjacent(Of :: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent(Of) ->
    OfZone = gmm_utils:owner_of(Of),
    case gmm_utils:zone_id() of
        OfZone -> graph:list_parents(Of);
        _ -> zone_client:list_adjacent(OfZone, Of)
    end.

-spec list_adjacent_reversed(Of :: binary()) -> {ok, list(binary())} | {error, any()}.
list_adjacent_reversed(Of) ->
    OfZone = gmm_utils:owner_of(Of),
    case gmm_utils:zone_id() of
        OfZone -> graph:list_children(Of);
        _ -> zone_client:list_adjacent_reversed(OfZone, Of)
    end.
