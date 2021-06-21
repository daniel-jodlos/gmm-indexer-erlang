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
    is_conflict/2,
    resource_exists/2
]).

-export([
    from_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req0, State) ->
    {ParsedParams, Req} =
        case maps:get(operation, State) of
            Op when Op =:= add; Op =:= permissions ->
                {cowboy_req:match_qs([
                    {from, nonempty}, {to, nonempty}, {permissions, nonempty},
                    {trace, [], undefined}, {successive, nonempty}
                ], Req0), Req0};
            delete ->
                {cowboy_req:match_qs([
                    {from, nonempty}, {to, nonempty}, {trace, [], undefined}, {successive, nonempty}
                ], Req0), Req0};
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
    #{from := From, to := To} = State,
    {ok, Result} = graph:edge_exists(From, To),
    {Result, Req, State}.

is_conflict(Req, State) ->
    Result = case maps:get(operation, State) of
                 add -> true;
                 _ -> false
             end,
    {Result, Req, State}.

%% POST handler
from_json(Req, State) ->
    Result = case maps:get(operation, State) of
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
                     graph:create_edge(From, To, Permissions);
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
