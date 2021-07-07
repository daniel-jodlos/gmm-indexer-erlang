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

init(Req = #{method := <<"GET">>}, State = #{operation := health_check}) ->
    {cowboy_rest, Req, State};
init(Req = #{method := <<"GET">>}, State = #{operation := index_ready}) ->
    {cowboy_rest, Req, State};
init(Req = #{method := <<"GET">>}, State = #{operation := instrumentation}) ->
    {cowboy_rest, Req, State};
init(Req = #{method := <<"PUT">>}, State = #{operation := instrumentation}) ->
    NewState = gmm_utils:parse_rest_params(Req, State, [{enabled, nonempty}],
        [{enabled, fun gmm_utils:parse_boolean/1}]),
    {cowboy_rest, Req, NewState};
init(Req = #{method := <<"PUT">>}, State = #{operation := indexation}) ->
    NewState = gmm_utils:parse_rest_params(Req, State, [{enabled, nonempty}],
        [{enabled, fun gmm_utils:parse_boolean/1}]),
    {cowboy_rest, Req, NewState};
init(Req0 = #{method := <<"POST">>}, State = #{operation := dependent_zones}) ->
    {Req, NewState} = gmm_utils:parse_rest_body(Req0, State, fun parse_dependent_zones/1),
    {cowboy_rest, Req, NewState};
init(Req, _) ->
    {cowboy_rest, Req, bad_request}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

%% @todo implement case for dependent_zones, if needed
resource_exists(Req, bad_request) ->
    {false, Req, bad_request};
resource_exists(Req, State) ->
    {true, Req, State}.

%% POST/PUT handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State = #{operation := instrumentation, enabled := Bool}) ->
    ok = set_instrumentation(Bool),
    {true, Req, State};
from_json(Req, State = #{operation := indexation, enabled := Bool}) ->
    ok = set_indexation(Bool),
    {true, Req, State};
from_json(Req0, State = #{operation := dependent_zones, body := List}) ->
    {ok, NewList} = set_dependent_zones(List),
    Req = cowboy_req:set_resp_body(gmm_utils:encode(#{<<"zones">> => NewList}), Req0),
    {true, Req, State}.

%% GET handler
to_json(Req, State = #{operation := health_check}) ->
    {gmm_utils:encode(true), Req, State};
to_json(Req, State = #{operation := index_ready}) ->
    {ok, Bool} = is_index_up_to_date(),
    {gmm_utils:encode(Bool), Req, State};
to_json(Req, State = #{operation := instrumentation}) ->
    {ok, Bool} = get_instrumentation(),
    {gmm_utils:encode(Bool), Req, State}.


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
%% @todo - event processor
-spec is_index_up_to_date() -> {ok, boolean()} | {error, any()}.
is_index_up_to_date() ->
    inbox:is_empty() and outbox:all_empty() and true.

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
                false -> {error, "Incorrect list"}
            end;
        _ -> {error, "Invalid JSON"}
    end.
