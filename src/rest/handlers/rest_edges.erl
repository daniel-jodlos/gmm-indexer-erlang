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

init(Req, State = #{operation := Op}) when Op == add; Op == update ->
    NewState = gmm_utils:parse_rest_params(Req, State,
        [{from, nonempty}, {to, nonempty}, {permissions, nonempty}, {trace, [], undefined}, {successive, nonempty}],
        [{from, fun gmm_utils:validate_vertex_id/1}, {to, fun gmm_utils:validate_vertex_id/1},
            {successive, fun gmm_utils:parse_boolean/1}]),
    {cowboy_rest, Req, NewState};
init(Req, State = #{operation := delete}) ->
    NewState = gmm_utils:parse_rest_params(Req, State,
        [{from, nonempty}, {to, nonempty}, {trace, [], undefined}, {successive, nonempty}],
        [{from, fun gmm_utils:validate_vertex_id/1}, {to, fun gmm_utils:validate_vertex_id/1},
            {successive, fun gmm_utils:parse_boolean/1}]),
    case NewState of
        bad_request -> {cowboy_rest, Req, bad_request};
        _ -> {cowboy_rest, Req, maps:merge(NewState, #{permissions => undefined})}
    end;
init(Req0, State = #{operation := bulk}) ->
    {Req, NewState} = gmm_utils:parse_rest_body(Req0, State, fun parse_bulk_request/1),
    {cowboy_rest, Req, NewState};
init(Req, _) ->
    {cowboy_rest, Req, bad_request}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% POST handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State = #{operation := Op, from := From, to := To, permissions := Permissions, trace := Trace,
        successive := Successive}) when Op == add; Op == update; Op == delete ->
    ok = execute_operation(Op, From, To, Permissions, Trace, Successive),
    {true, Req, State};
%%from_json(Req, State = #{operation := bulk, body := #{
%%        src_zone := _SrcZone, dst_zone := _DstZone, successive = _Successive, edges := _Edges}}) ->
from_json(Req, State = #{operation := bulk, body := _}) ->
    %% @todo execute bulk request parsed in init/2
    {error, _} = execute_bulk_request(),
    {true, Req, State}.

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

-spec parse_bulk_request(binary()) -> {ok, map()} | {error, any()}.
parse_bulk_request(Bin) ->
    case gmm_utils:decode(Bin) of
        #{<<"sourceZone">> := SourceZone, <<"destinationZone">> := DestinationZone, <<"successive">> := SuccessiveBin,
                <<"edges">> := List} when is_binary(SourceZone), is_binary(DestinationZone), is_list(List) ->
            ParsedEdges = lists:map(fun parse_edge_string/1, List),
            case gmm_utils:parse_boolean(SuccessiveBin) of
                {ok, Successive} ->
                    case lists:all(fun({ok, _}) -> true; (_) -> false end, ParsedEdges) of
                        true -> {_, ParsedList} = lists:unzip(ParsedEdges),
                            {ok, #{
                                src_zone => SourceZone, dst_zone => DestinationZone,
                                successive => Successive, edges => ParsedList
                            }};
                        false -> {error, "Invalid JSON"}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        _ -> {error, "Invalid JSON"}
    end.


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
