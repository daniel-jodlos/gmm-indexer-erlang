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
    resource_exists/2,
    execute_operation/6
]).

-export([
    from_json/2
]).

-include("records.hrl").

%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State = #{operation := Op}) when Op == add; Op == update ->
    NewState = parser:parse_rest_params(Req, State,
        [{from, nonempty}, {to, nonempty}, {permissions, nonempty}, {trace, [], <<"">>}, {successive, nonempty}],
        [{from, fun gmm_utils:validate_vertex_id/1}, {to, fun gmm_utils:validate_vertex_id/1},
            {successive, fun parser:parse_boolean/1}]),
    {cowboy_rest, Req, NewState};
init(Req, State = #{operation := delete}) ->
    NewState = parser:parse_rest_params(Req, State,
        [{from, nonempty}, {to, nonempty}, {trace, [], <<"">>}, {successive, nonempty}],
        [{from, fun gmm_utils:validate_vertex_id/1}, {to, fun gmm_utils:validate_vertex_id/1},
            {successive, fun parser:parse_boolean/1}]),
    case NewState of
        bad_request -> {cowboy_rest, Req, bad_request};
        _ -> {cowboy_rest, Req, maps:merge(NewState, #{permissions => undefined})}
    end;
init(Req0, State = #{operation := bulk}) ->
    {Req, NewState} = parser:parse_rest_body(Req0, State, fun parse_bulk_request/1),
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
from_json(Req, State = #{operation := Op, from := From, to := To, permissions := Permissions, trace := TraceParam,
        successive := Successive}) when Op == add; Op == update; Op == delete ->
    Trace = get_trace(TraceParam),
    ok = execute_operation(Op, From, To, Permissions, Trace, Successive),
    {true, Req, State};
from_json(Req, State = #{operation := bulk, body := #{src_zone := SrcZone, dst_zone := DstZone, successive := Successive, edges := Edges}}) ->
    ok = execute_bulk_request(SrcZone, DstZone, Successive, Edges),
    {true, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_edge_string(binary()) -> {ok, map()} | {error, any()}.
parse_edge_string(Bin) when is_binary(Bin) ->
    try
        case gmm_utils:split_bin(Bin) of
            [From, To, Permissions, Trace] when byte_size(From) > 0, byte_size(To) > 0, byte_size(Permissions) > 0 ->
                {ok, #{from => From, to => To, permissions => Permissions, trace => Trace}};
            _ -> {error, {incorrect_edge_string, Bin}}
        end
    catch
        error:Reason -> {error, {Reason, Bin}};
        Class:Reason -> {error, {{Class, Reason}, Bin}}
    end.

-spec parse_bulk_request(binary()) -> {ok, map()} | {error, any()}.
parse_bulk_request(Bin) when is_binary(Bin) ->
    case gmm_utils:decode(Bin) of
        #{<<"sourceZone">> := SourceZone, <<"destinationZone">> := DestinationZone, <<"successive">> := Successive,
                <<"edges">> := List} when is_binary(SourceZone), is_binary(DestinationZone), is_list(List), is_boolean(Successive) ->
            ParsedEdges = lists:map(fun parse_edge_string/1, List),
            Aggregated =
                lists:foldr(
                    fun
                        ({error, R}, {error, RList}) -> {error, [R | RList]};
                        ({error, R}, {ok, _}) -> {error, R};
                        ({ok, _}, {error, RList}) -> {error, RList};
                        ({ok, ParsedEdge}, {ok, ParsedList}) -> {ok, [ParsedEdge | ParsedList]}
                    end,
                    {ok, []}, ParsedEdges),
            case Aggregated of
                {ok, EdgesList} ->
                    {ok, #{
                        src_zone => SourceZone, dst_zone => DestinationZone,
                        successive => Successive, edges => EdgesList
                    }};
                {error, R} -> {error, R}
            end;
        _ -> {error, {invalid_bulk_json, Bin}}
    end.

get_trace(Trace) when is_binary(Trace) ->
    case byte_size(Trace) of
        0 -> gmm_utils:uuid();
        _ -> Trace
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
            false -> {error, {conditions_failed, {Op, From, To, false}}}
        end,
    case SuccessiveCallResult of
        ok -> modify_state(Op, From, To, Permissions, false, gmm_utils:owner_of(From) == gmm_utils:owner_of(To));
        {error, Reason} -> {error, Reason}
    end;
execute_locally(Op, From, To, Permissions, _Trace, true) ->
    case conditions_met(Op, From, To, true) of
        true -> modify_state(Op, From, To, Permissions, true, gmm_utils:owner_of(From) == gmm_utils:owner_of(To));
        false -> {error, {conditions_failed, {Op, From, To, true}}}
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

-spec conditions_met_bulk(SrcZone :: binary(), DstZone :: binary(), Successive :: boolean(), Edges :: list(map())) -> boolean().
conditions_met_bulk(SrcZone, DstZone, Successive, Edges) ->
    Parent = self(),

    Pids = lists:map(fun(#{from := FromName, to := ToName, permissions := _Permissions, trace := _Trace}) ->
        spawn(fun() ->
            Result =
                try From = gmm_utils:create_vertex_id(SrcZone,  FromName),
                    To = gmm_utils:create_vertex_id(DstZone, ToName),
                    Vertex = case Successive of false -> From; true -> To end,
                    case [graph:vertex_exists(Vertex), graph:edge_exists(From, To)] of
                        [{ok, true}, {ok, false}] -> true;
                        _ -> false
                    end
                catch Type:Reason:Stacktrace -> {'$pmap_error', self(), Type, Reason, Stacktrace} end,
            Parent ! {self(), Result} end) end, Edges),

    % GATHERING RESULTS
    parallel_utils:gather(conditions, Pids).

-spec modify_state_bulk(SrcZone :: binary(), DstZone :: binary(), Edges :: list(map()), Successive :: boolean(), OneZoneOperation :: boolean()) -> ok | {error, any()}.
modify_state_bulk(_, _, _, false, true) ->
    ok;
modify_state_bulk(SrcZone, DstZone, Edges, _, _)->
    Parent = self(),

    Pids = lists:map(
        fun(#{from := FromName, to := ToName, permissions := Permissions, trace := _Trace}) ->
            spawn(
                fun() ->
                    Result = try graph:create_edge(gmm_utils:create_vertex_id(SrcZone,  FromName), gmm_utils:create_vertex_id(DstZone, ToName), Permissions)
                            catch Type:Reason:Stacktrace -> {'$pmap_error', self(), Type, Reason, Stacktrace} end,
                    Parent ! {self(), Result}
                end)
        end, Edges),

    % GATHERING RESULTS
    parallel_utils:gather(no_conditions, Pids).


-spec execute_locally_bulk(SrcZone :: binary(), DstZone :: binary(), Successive :: boolean(), Edges :: list(map())) -> ok | {error, any()}.
execute_locally_bulk(SrcZone, DstZone, false, Edges) ->
    SuccessiveCallResult =
        case conditions_met_bulk(SrcZone, DstZone, false, Edges) of
            true -> execute_bulk_request(SrcZone, DstZone, true, Edges);
            false -> {error, {conditions_failed, {SrcZone, DstZone, false, Edges}}}
        end,
        case SuccessiveCallResult of
            ok -> modify_state_bulk(SrcZone, DstZone, Edges, false, SrcZone == DstZone);
            {error, Reason} -> {error, Reason}
        end;
execute_locally_bulk(SrcZone, DstZone, true, Edges) ->
    case conditions_met_bulk(SrcZone, DstZone, true, Edges) of
        true -> modify_state_bulk(SrcZone, DstZone, Edges, true, SrcZone == DstZone);
        false -> {error, {conditions_failed, {SrcZone, DstZone, true, Edges}}}
    end.


-spec execute_bulk_request(SrcZone :: binary(), DstZone :: binary(), Successive :: boolean(), Edges :: list(map())) -> ok | {error, any()}.
execute_bulk_request(SrcZone, DstZone, Successive, Edges) ->
    VerticesZone =
        case Successive of
            false -> SrcZone;
            true -> DstZone
        end,
    ZoneId = gmm_utils:zone_id(),
    case VerticesZone of
        ZoneId -> execute_locally_bulk(SrcZone, DstZone, Successive, Edges);
        _ ->
            EdgesBin = lists:map(
                fun(#{from := FromName, to := ToName, permissions := Permissions, trace := Trace}) ->
                    << FromName/binary, "/", ToName/binary, "/", Permissions/binary, "/", Trace/binary >>
                end, Edges),

            zone_client:add_edges(VerticesZone, #{<<"sourceZone">> => SrcZone, <<"destinationZone">> => DstZone, <<"successive">> => Successive, <<"edges">> => EdgesBin})
    end.

