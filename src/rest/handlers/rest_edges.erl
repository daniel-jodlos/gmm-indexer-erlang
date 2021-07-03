%%%-------------------------------------------------------------------
%% @doc
%%  @todo Incorporate edges between zones
%%  Implements API for manipulating edges.
%% @end
%%%-------------------------------------------------------------------

-module(rest_edges).
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

init(Req0, State) ->
    Operation = maps:get(operation, State),
    {ParsedParams, Req} =
        case Operation of
            Op when Op =:= add; Op =:= update ->
                {cowboy_req:match_qs([
                    {from, nonempty}, {to, nonempty}, {permissions, nonempty},
                    {trace, [], undefined}, {successive, nonempty}
                ], Req0), Req0};
            delete ->
                {maps:merge(cowboy_req:match_qs([
                    {from, nonempty}, {to, nonempty}, {trace, [], undefined}, {successive, nonempty}
                ], Req0), #{permissions => undefined}), Req0};
            bulk ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                ParsedJson = gmm_utils:decode(Data),
                {ok, Map} = parse_bulk_request(ParsedJson),
                {#{bulk_request => Map}, Req1}
        end,
    if
        Operation == add; Operation == update; Operation == delete ->
            ok = gmm_utils:validate_vertex_id(maps:get(from, ParsedParams)),
            ok = gmm_utils:validate_vertex_id(maps:get(to, ParsedParams));
        true -> ok
    end,
    NewState = maps:merge(State,
        case maps:find(successive, ParsedParams) of
            {ok, SuccessiveBin} ->
                {ok, Successive} = gmm_utils:parse_boolean(SuccessiveBin),
                maps:update(successive, Successive, ParsedParams);
            _ -> ParsedParams
        end),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% POST handler
from_json(Req, State) ->
    Result =
        case maps:get(operation, State) of
            Op when Op == add; Op == update; Op == delete ->
                #{from := From, to := To, permissions := Permissions, trace := Trace, successive := Successive} = State,
                execute_operation(Op, From, To, Permissions, Trace, Successive);
            bulk ->
                %% @todo execute bulk request parsed in init/2
                #{src_zone := _SourceZone, dst_zone := _DestinationZone, successive := _Successive, edges := _Edges} =
                    maps:get(bulk_request, State),
                execute_bulk_request()
        end,
    Flag =
        case Result of
            ok -> true;
            {error, _} -> false
        end,
    {Flag, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_edge_string(binary()) -> {ok, map()} | {error, any()}.
parse_edge_string(Bin) when is_binary(Bin) ->
    try
        case gmm_utils:split_bin(Bin) of
            [From, To, Permissions, Trace] when byte_size(From) > 0, byte_size(To) > 0, byte_size(Permissions) > 0 ->
                ok = gmm_utils:validate_vertex_id(From),
                ok = gmm_utils:validate_vertex_id(To),
                {ok, #{from => From, to => To, permissions => Permissions, trace => Trace}};
            _ -> {error, "Incorrect edge string"}
        end
    catch _:_ -> {error, "Invalid JSON"} end;
parse_edge_string(_) ->
    {error, "Argument is not a binary"}.

-spec parse_bulk_request(any()) -> {ok, map()} | {error, any()}.
parse_bulk_request(#{<<"sourceZone">> := SourceZone, <<"destinationZone">> := DestinationZone,
                        <<"successive">> := SuccessiveBin, <<"edges">> := List})
        when is_binary(SourceZone), is_binary(DestinationZone), is_list(List) ->
    ParsedEdges = lists:map(fun parse_edge_string/1, List),
    case {gmm_utils:parse_boolean(SuccessiveBin), lists:all(fun({ok, _}) -> true; (_) -> false end, ParsedEdges)} of
        {{ok, Successive}, true} ->
            {_, ParsedList} = lists:unzip(ParsedEdges),
            {ok, #{src_zone => SourceZone, dst_zone => DestinationZone, successive => Successive, edges => ParsedList}};
        _ -> {error, "Invalid JSON"}
    end;
parse_bulk_request(_) ->
    {error, "Invalid JSON"}.


%%%---------------------------
%% single operation executor
%%%---------------------------

-spec execute_operation(Op :: atom(), From :: binary(), To :: binary(),
    Permissions :: permissions() | undefined, Trace :: binary(), Successive :: boolean())
        -> ok | {error, any()}.
execute_operation(Op, From, To, Permissions, Trace, Successive) ->
    VertexZone =
        case Successive of
            false -> gmm_utils:owner_of(From);
            true -> gmm_utils:owner_of(To)
        end,
    ZoneId = gmm_utils:zone_id(),
    case VertexZone of
        ZoneId -> execute_locally(Op, From, To, Permissions, Trace, Successive);
        _ -> redirect(VertexZone, Op, From, To, Permissions, Trace, Successive)
    end.


-spec redirect(Zone :: binary(), Op :: atom(), From :: binary(), To :: binary(), Permissions :: permissions(),
    Trace :: binary(), Successive :: boolean()) -> ok | {error, any()}.
redirect(Zone, Op, From, To, Permissions, Trace, Successive) ->
    case Op of
        add -> zone_client:add_edge(Zone, From, To, Permissions, Trace, Successive);
        update -> zone_client:set_permissions(Zone, From, To, Permissions, Trace, Successive);
        delete -> zone_client:remove_edge(Zone, From, To, Trace, Successive)
    end.


-spec execute_locally(Op :: atom(), From :: binary(), To :: binary(), Permissions :: permissions(),
    Trace :: binary(), Successive :: boolean()) -> ok | {error, any()}.
execute_locally(Op, From, To, Permissions, Trace, false) ->
    SuccessiveCallResult =
        case conditions_met(Op, From, To, false) of
            true -> execute_operation(Op, From, To, Permissions, Trace, true);
            false -> {error, "Conditions failed"}
        end,
    case SuccessiveCallResult of
        ok -> modify_state(Op, From, To, Permissions, false, gmm_utils:owner_of(From) == gmm_utils:owner_of(To));
        {error, Reason} -> {error, Reason}
    end;
execute_locally(Op, From, To, Permissions, _Trace, true) ->
    case conditions_met(Op, From, To, true) of
        true -> modify_state(Op, From, To, Permissions, true, gmm_utils:owner_of(From) == gmm_utils:owner_of(To));
        false -> {error, "Conditions failed"}
    end.


-spec conditions_met(Op :: atom(), From :: binary(), To :: binary(), Successive :: boolean()) -> boolean().
conditions_met(Op, From, To, Successive) ->
    EdgeCond = (Op == update) or (Op == delete),
    Vertex = case Successive of false -> From; true -> To end,
    case [graph:vertex_exists(Vertex), graph:edge_exists(From, To)] of
        [{ok, true}, {ok, EdgeCond}] -> true;
        _ -> false
    end.


-spec modify_state(Op :: atom(), From :: binary(), To :: binary(), Permissions :: permissions(),
    Successive :: boolean(), OneZoneOperation :: boolean()) -> ok | {error, any()}.
modify_state(_, _, _, _, false, true) ->
    ok;
modify_state(add, From, To, Permissions, _, _) ->
    graph:create_edge(From, To, Permissions);
modify_state(update, From, To, Permissions, _, _) ->
    graph:update_edge(From, To, Permissions);
modify_state(delete, From, To, _, _, _) ->
    graph:remove_edge(From, To).


%%%---------------------------
%% bulk operation executor
%%%---------------------------

-spec execute_bulk_request() -> ok | {error, any()}.
execute_bulk_request() ->
    {error, "Not implemented"}.
