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
init(Req0 = #{method := <<"PUT">>}, State = #{operation := instrumentation}) ->
    {Req, #{body := Enabled}} = gmm_utils:parse_rest_body(Req0, State, fun gmm_utils:parse_boolean/1),
    {cowboy_rest, Req, maps:merge(State, #{enabled => Enabled})};
init(Req0 = #{method := <<"PUT">>}, State = #{operation := indexation}) ->
    {Req, #{body := Enabled}} = gmm_utils:parse_rest_body(Req0, State, fun gmm_utils:parse_boolean/1),
    {cowboy_rest, Req, maps:merge(State, #{enabled => Enabled})};
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

resource_exists(Req, bad_request) ->
    {false, Req, bad_request};
resource_exists(Req, State) ->
    {true, Req, State}.

%% POST/PUT handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State = #{operation := instrumentation, enabled := Bool}) ->
    gmm_utils:set_instrumentation_enabled(Bool),
    {true, Req, State};
from_json(Req, State = #{operation := indexation, enabled := Bool}) ->
    gmm_utils:set_indexation_enabled(Bool),
    {true, Req, State};
from_json(Req0, State = #{operation := dependent_zones, body := List}) ->
    {ok, NewList} = get_dependent_zones(List),
    Req = cowboy_req:set_resp_body(gmm_utils:encode(#{<<"zones">> => NewList}), Req0),
    {true, Req, State}.

%% GET handler
to_json(Req, State = #{operation := health_check}) ->
    {gmm_utils:empty_json(), Req, State};
to_json(Req, State = #{operation := index_ready}) ->
    Bool = is_index_up_to_date(),
    {gmm_utils:encode(Bool), Req, State};
to_json(Req, State = #{operation := instrumentation}) ->
    Bool = gmm_utils:get_instrumentation_enabled(),
    {gmm_utils:encode(Bool), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

%%% Check if index is correct, which requires 3 conditions:
%%%  1) inbox is empty, 2) outbox is empty, 3) there are no currently processed events
%% @todo - check if event processor isn't doing anything
-spec is_index_up_to_date() -> boolean().
is_index_up_to_date() ->
    inbox:is_empty() and outbox:all_empty() and true.

%% @todo test it
-spec get_dependent_zones(list(binary())) -> {ok, list(binary())} | {error, any()}.
get_dependent_zones(ExcludeList) ->
    {ok, AllZones} = graph:all_zones(),

    DirectlyDependent = sets:subtract(sets:from_list(AllZones), sets:from_list(ExcludeList)),
    NewExcludeList = ExcludeList ++ sets:to_list(DirectlyDependent),

    Dependent = lists:foldl(
        fun(Zone, AccSet) ->
            {ok, #{<<"zones">> := List}} = zone_client:get_dependent_zones(Zone, NewExcludeList),
            sets:union(AccSet, sets:from_list(List))
        end,
        DirectlyDependent,
        sets:to_list(DirectlyDependent)
    ),
    {ok, sets:to_list(Dependent)}.


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
