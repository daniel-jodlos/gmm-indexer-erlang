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
    {ParsedParams, Req} =
        case maps:get(operation, State) of
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
    NewState = maps:merge(State, ParsedParams),
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
                ok
        end,
    Flag = case Result of
               ok -> true;
               {error, _} -> false
           end,
    {Flag, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_edge_string(binary()) -> {ok, map()} | {error, any()}.
parse_edge_string(Bin) when is_binary(Bin) ->
    case gmm_utils:split_bin(Bin) of
        {ok, [From, To, Permissions, Trace]} ->
            {ok, #{from => From, to => To, permissions => Permissions, trace => Trace}};
        _ -> {error, "Incorrect edge string"}
    end;
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
            {ok, #{sourceZone => SourceZone, destinationZone => DestinationZone, successive => Successive, edges => ParsedList}};
        _ -> {error, "Invalid JSON"}
    end;
parse_bulk_request(_) ->
    {error, "Invalid JSON"}.


%%%---------------------------
%% operation executor
%%%---------------------------

-spec execute_operation(Op :: atom(), From :: binary(), To :: binary(), Permissions :: binary() | undefined,
    Trace :: binary(), Successive :: boolean()) -> ok | {error, any()}.
execute_operation(Op, From, To, Permissions, Trace, false) ->
    try
        {ok, {FromZone, _}} = gmm_utils:split_vertex_id(From),
        ZoneId = list_to_binary(?ZONE_ID),
        if
            %% @todo ZONE_ID is list "zone0", and not a binary <<"zone0">> - I need to do something about it
            FromZone =/= ZoneId -> throw({return,
                case Op of
                    add -> zone_client:add_edge(FromZone, From, To, Permissions, Trace, false);
                    update -> zone_client:set_permissions(FromZone, From, To, Permissions, Trace, false);
                    delete -> zone_client:remove_edge(FromZone, From, To, Trace, false)
                end});
            true -> ok
        end,
        EdgeCond = (Op == delete) or (Op == update),
        [{ok, true}, {ok, EdgeCond}] = [graph:vertex_exists(From), graph:edge_exists(From, To)],
        ok = execute_operation(Op, From, To, Permissions, Trace, true),
        case gmm_utils:split_vertex_id(To) of
            {ok, {FromZone, _}} -> ok; %% operation was already done on this zone in successive call
            {ok, {_, _}} ->
                case Op of
                    add -> graph:create_edge(From, To, Permissions);
                    update -> graph:update_edge(From, To, Permissions);
                    delete -> graph:remove_edge(From, To)
                end
        end
    catch
        throw:{return, Result} -> Result;
        _:_ -> {error, something}
    end;
execute_operation(Op, From, To, Permissions, Trace, true) ->
    try
        {ok, {ToZone, _}} = gmm_utils:split_vertex_id(To),
        ZoneId = list_to_binary(?ZONE_ID),
        if
            ToZone =/= ZoneId -> throw({return,
                case Op of
                    add -> zone_client:add_edge(ToZone, From, To, Permissions, Trace, true);
                    update -> zone_client:set_permissions(ToZone, From, To, Permissions, Trace, true);
                    delete -> zone_client:remove_edge(ToZone, From, To, Trace, true)
                end});
            true -> ok
        end,
        EdgeCond = (Op == delete) or (Op == update),
        [{ok, true}, {ok, EdgeCond}] = [graph:vertex_exists(To), graph:edge_exists(From, To)],
        case Op of
            add -> graph:create_edge(From, To, Permissions);
            update -> graph:update_edge(From, To, Permissions);
            delete -> graph:remove_edge(From, To)
        end
    catch
        throw:{return, Result} -> Result;
        _:_ -> {error, something}
    end.
