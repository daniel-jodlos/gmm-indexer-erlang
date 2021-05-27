%%%-------------------------------------------------------------------
%% @doc
%%  @todo
%%  Implements API for setting/getting various settings
%% @end
%%%-------------------------------------------------------------------

-module(rest_meta_info).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    allowed_methods/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {Req, ParsedParams} =
        case {maps:get(op, State), Method} of
            {health_check, <<"GET">>} -> {Req0, #{}};
            {index_ready, <<"GET">>} -> {Req0, #{}};
            {dependent_zones, <<"POST">>} ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                {Req1, #{body => Data}};
            {instrumentation, <<"GET">>} -> {Req0, #{}};
            {instrumentation, <<"PUT">>} -> {Req0, cowboy_req:match_qs([{enabled, nonempty}], Req0)};
            {indexation, <<"PUT">>} -> {Req0, cowboy_req:match_qs([{enabled, nonempty}], Req0)}
        end,
    NewState = maps:merge(maps:put(method, Method, State), ParsedParams),
%%    io:format("State: ~p\n", [NewState]),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

%% @todo implement case for dependent_zones, if needed
resource_exists(Req, State) ->
    {true, Req, State}.

%% POST/PUT handler
from_json(Req, State) ->
    Result = case maps:get(op, State) of
                 instrumentation ->
                     {ok, Bool} = parse_boolean(maps:get(enabled, State)),
                     set_instrumentation(Bool);
                 indexation ->
                     {ok, Bool} = parse_boolean(maps:get(enabled, State)),
                     set_indexation(Bool);
                 dependent_zones ->
                     case parse_dependent_zones(maps:get(body, State)) of
                         {ok, List} -> set_dependent_zones(List);
                         {error, _} -> false
                     end
             end,
    {Result, Req, State}.

%% GET handler
to_json(Req, State) ->
    %% Result should be boolean
    Result = case maps:get(op, State) of
                 health_check -> true;
                 index_ready -> get_index_ready();
                 instrumentation -> get_instrumentation()
             end,
    {json_utils:encode(Result), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

%% @todo
set_instrumentation(_Bool) ->
    true.

%% @todo
get_instrumentation() ->
    false.

%% @todo
set_indexation(_Bool) ->
    true.

%% @todo
get_index_ready() ->
    false.

%% @todo
set_dependent_zones(List) ->
    {true, json_utils:encode(#{<<"zones">> => List})}.


parse_dependent_zones(Data) ->
    case json_utils:decode(Data) of
        List when is_list(List) ->
            case lists:all(fun(X) -> is_binary(X) end, List) of
                true -> {ok, List};
                false -> {error, "Some element is not binary string"}
            end;
        _ -> {error, "Json is not not a list"}
    end.

parse_boolean(Bin) ->
    try
        Atom = binary_to_atom(Bin),
        case Atom of
            Bool when is_boolean(Bool) -> {ok, Bool};
            _ -> {error, not_a_bool}
        end
    catch _:_ ->
        {error, not_a_bool}
    end.
