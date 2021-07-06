%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for enabling/disabling various settings
%% @end
%%%-------------------------------------------------------------------

-module(rest_meta_info).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2
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
        case {maps:get(operation, State), Method} of
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
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

%% @todo implement case for dependent_zones, if needed
resource_exists(Req, State) ->
    {true, Req, State}.

%% POST/PUT handler
from_json(Req, State) ->
    Result =
        case maps:get(operation, State) of
            instrumentation ->
                parse_bool_and_execute(maps:get(enabled, State), fun set_instrumentation/1);
            indexation ->
                parse_bool_and_execute(maps:get(enabled, State), fun set_indexation/1);
            dependent_zones ->
                case parse_dependent_zones(maps:get(body, State)) of
                    {ok, List} -> set_dependent_zones(List);
                    {error, Reason} -> {error, Reason}
                end
        end,
    case Result of
        ok -> {true, Req, State};
        {ok, Value} ->
            Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Value), Req),
            {true, Req1, State};
        _ -> {false, Req, State}
    end.

%% GET handler
to_json(Req, State) ->
    %% Result should be boolean
    Result =
        case maps:get(operation, State) of
            health_check -> {ok, true};
            index_ready -> is_index_up_to_date();
            instrumentation -> get_instrumentation()
        end,
    {ok, Value} = Result,
    {gmm_utils:encode(Value), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

%% @todo
-spec set_instrumentation(boolean()) -> ok | {error, any()}.
set_instrumentation(_Bool) ->
    ok.

%% @todo
-spec get_instrumentation() -> {ok, boolean()} | {error, any()}.
get_instrumentation() ->
    {ok, false}.

%% @todo
-spec set_indexation(boolean()) -> ok | {error, any()}.
set_indexation(_Bool) ->
    ok.

%%% Check if index is correct, which requires 3 conditions:
%%%  1) inbox is empty, 2) outbox is empty, 3) there are no currently processed events
%% @todo
-spec is_index_up_to_date() -> {ok, boolean()} | {error, any()}.
is_index_up_to_date() ->
    {ok, false}.

%% @todo
-spec set_dependent_zones(list(binary())) -> {ok, map()} | {error, any()}.
set_dependent_zones(List) ->
    {ok, #{<<"zones">> => List}}.


-spec parse_dependent_zones(binary()) -> {ok, list(binary())} | {error, any()}.
parse_dependent_zones(Data) ->
    case gmm_utils:decode(Data) of
        List when is_list(List) ->
            case lists:all(fun is_binary/1, List) of
                true -> {ok, List};
                false -> {error, "Some element is not binary string"}
            end;
        _ -> {error, "Json is not not a list"}
    end.

-spec parse_bool_and_execute(binary(), fun((boolean()) -> ok | {error, any()})) -> ok | {error, any()}.
parse_bool_and_execute(Arg, Fun) ->
    case gmm_utils:parse_boolean(Arg) of
        {ok, Bool} -> Fun(Bool);
        {error, Reason} -> {error, Reason}
    end.
