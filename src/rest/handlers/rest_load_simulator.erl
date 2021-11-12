%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for request of load simulation
%% @end
%%%-------------------------------------------------------------------

-module(rest_load_simulator).
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

init(Req0, State) ->
    {Req, NewState} = parser:parse_rest_body(Req0, State, fun parse_load_body/1),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

operation_type(Type)->
    case Type of
        <<"a">> -> create;
        <<"p">> -> update;
        <<"r">> -> delete
    end.

%% POST handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State) ->
    Parent = self(),

    {Updates, NotUpdates} = lists:splitwith(fun(#{op_type := Type, from := _From, to := _To, permissions := _Permissions, trace := _Trace}) ->
        Operation = operation_type(Type),
        case Operation of
            update -> true;
            _ -> false
        end end, maps:get(body, State)),

    lists:foreach(
        fun(#{op_type := Type, from := From, to := To, permissions := Permissions, trace := Trace}) ->
            Operation = operation_type(Type),
            rest_edges:execute_operation(Operation, From, To, Permissions, Trace, false)
        end, NotUpdates),

    Pids = lists:map(
        fun(#{op_type := _Type, from := From, to := To, permissions := Permissions, trace := Trace}) ->
            spawn(fun() ->
                Result =
                    try rest_edges:execute_operation(update, From, To, Permissions, Trace, false)
                    catch Type:Reason:Stacktrace -> {'$pmap_error', self(), Type, Reason, Stacktrace} end,
                Parent ! {self(), Result} end)
        end, Updates),

    % POTENTIAL PIDS FILTER WILL BE REQUIRED

    parallel_utils:gather(no_conditions, Pids),
    {true, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_load_body(binary()) -> {ok, list(map())} | {error, any()}.
parse_load_body(Bin) ->
    case gmm_utils:decode(Bin) of
        #{<<"ops">> := List} when is_list(List) ->
            Validator =
                fun(#{<<"t">> := Type, <<"f">> := From, <<"to">> := To, <<"p">> := Permissions, <<"tr">> := Trace})
                        when Type =:= <<"a">>; Type =:= <<"r">>; Type =:= <<"p">> ->
                    {ok, #{op_type => Type, from => From, to => To, permissions => Permissions, trace => Trace}};
                    (_) -> {error, "Invalid JSON"}
                end,
            lists:foldr(
                fun
                    (_, Err={error, _}) -> Err;
                    (Err={error, _}, _) -> Err;
                    ({ok, Op}, {ok, Acc}) -> {ok, [Op | Acc]}
                end,
                {ok, []},
                lists:map(Validator, List)
            );
        _ -> {error, "Invalid JSON"}
    end.
