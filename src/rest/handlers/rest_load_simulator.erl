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
    {Req, NewState} = gmm_utils:parse_rest_body(Req0, State, fun parse_load_body/1),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State) ->
    lists:map(fun(#{op_type := Type, from := From, to := To, permissions := Permissions, trace := Trace}) ->
        spawn(fun() ->
                Operation = case Type of
                                <<"add">> -> create;
                                <<"perm">> -> update;
                                <<"del">> -> delete
                            end,
                rest_edges:execute_operation(Operation, From, To, Permissions, Trace, false) end)
              end, maps:get(body, State)),

    {true, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_load_body(binary()) -> {ok, map()} | {error, any()}.
parse_load_body(Bin) ->
    case gmm_utils:decode(Bin) of
        #{<<"ops">> := List} when is_list(List) ->
            Validator =
                fun(#{<<"t">> := Type, <<"f">> := From, <<"to">> := To, <<"p">> := Permissions, <<"tr">> := Trace})
                        when Type =:= <<"a">>; Type =:= <<"r">>; Type =:= <<"p">> ->
                    {ok, #{op_type => Type, from => From, to => To, permissions => Permissions, trace => Trace}};
                    (_) -> {error, "Invalid JSON"}
                end,
            ValidatedList = lists:map(Validator, List),
            case lists:all(fun({ok, _}) -> true; (_) -> false end, ValidatedList) of
                true -> {ok, ValidatedList};
                false -> {error, "Parsing JSON error"}
            end;
        _ -> {error, "Invalid JSON"}
    end.
