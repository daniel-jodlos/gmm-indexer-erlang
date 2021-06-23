%%%-------------------------------------------------------------------
%% @doc
%%  @todo
%%  Implements API for handling events sent by other zones
%% @end
%%%-------------------------------------------------------------------

-module(rest_events).
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
    #{operation := Operation} = State,
    {ParsedParams, Req} =
        case {Method, Operation} of
            {<<"POST">>, single_event} ->
                Params = cowboy_req:match_qs([{id, nonempty}], Req0),
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                Event = gmm_utils:decode(Data),
                ok = validate_event(Event),
                {maps:merge(Params, #{body => Event}), Req1};
            {<<"POST">>, bulk} ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                BulkEvents = gmm_utils:decode(Data),
                ok = validate_bulk_events(BulkEvents),
                #{<<"messages">> := MessagesList} = BulkEvents,
                {#{body => MessagesList}, Req1};
            {<<"GET">>, stats} ->
                {#{}, Req0}
        end,
    NewState = maps:merge(State, ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% todo
%% POST handler
from_json(Req, State) ->
    Result =
        case maps:get(operation, State) of
            single_event -> io:format("~p\n", [maps:get(body, State)]), ok;
            bulk -> io:format("~p\n", [maps:get(body, State)]), {error, ""}
        end,
    SuccessFlag =
        case Result of
            ok -> true;
            {error, _} -> false
        end,
    {SuccessFlag, Req, State}.

%% @todo
%% GET handler
to_json(Req, State) ->
    Map = #{
        <<"processing">> => 0,
        <<"processingNanos">> => 0.0,
        <<"processingByType">> => #{
            %% not all fields required
            <<"user">> => 0,
            <<"group">> => 0,
            <<"space">> => 0,
            <<"provider">> => 0
        },
        <<"queued">> => 0,
        <<"outbox">> => 0,
        <<"total">> => 0,
        <<"load1">> => 0.0,
        <<"load5">> => 0.0,
        <<"load15">> => 0.0
    },
    {gmm_utils:encode(Map), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec validate_event(any()) -> ok | {error, any()}.
validate_event(#{<<"type">> := Type, <<"trace">> := Trace, <<"sender">> := Sender,
                <<"originalSender">> := OriginalSender, <<"effectiveVertices">> := EffectiveVertices}) when
        is_binary(Type), is_binary(Trace), is_binary(Sender), is_binary(OriginalSender), is_list(EffectiveVertices) ->
    Conditions = [
        ok == gmm_utils:validate_vertex_id(Sender),
        ok == gmm_utils:validate_vertex_id(OriginalSender),
        lists:all(fun(ok) -> true; (_) -> false end, lists:map(fun gmm_utils:validate_vertex_id/1, EffectiveVertices))
    ],
    case lists:all(fun(X) -> X end, Conditions) of
        true -> ok;
        false -> {error, "One of effective vertex's ID is invalid"}
    end;
validate_event(_) ->
    {error, "Event's JSON in a wrong format"}.

-spec validate_bulk_events(any()) -> ok | {error, any()}.
validate_bulk_events(#{<<"messages">> := List}) when is_list(List) ->
    Fun =
        fun(#{<<"vn">> := Vn, <<"e">> := Event}) when is_binary(Vn) ->
            case validate_event(Event) of
                ok -> true;
                {error, _} -> false
            end;
        (_) ->
            false
        end,
    case lists:all(Fun, List) of
        true -> ok;
        false -> {error, "One of events is not in correct format"}
    end;
validate_bulk_events(_) ->
    {error, "Bulk event's JSON in a wrong format"}.
