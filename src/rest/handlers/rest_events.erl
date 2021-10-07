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

-include("records.hrl").


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req = #{method := <<"GET">>}, State = #{operation := stats}) ->
    {cowboy_rest, Req, State};
init(Req0 = #{method := <<"POST">>}, State0 = #{operation := single_event}) ->
    State1 = gmm_utils:parse_rest_params(Req0, State0, [{id, nonempty}], [{id, fun gmm_utils:validate_vertex_id/1}]),
    {Req, State2} = gmm_utils:parse_rest_body(Req0, State1, fun parse_event/1),
    {cowboy_rest, Req, State2};
init(Req0 = #{method := <<"POST">>}, State0 = #{operation := bulk}) ->
    {Req, NewState} = gmm_utils:parse_rest_body(Req0, State0, fun parse_bulk_events/1),
    {cowboy_rest, Req, NewState};
init(Req, _) ->
    {cowboy_rest, Req, bad_request}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, bad_request) ->
    {false, Req, bad_request};
resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State = #{operation := single_event, id := Vertex, body := Event}) ->
    ok = inbox:post(Vertex, Event),
    {true, Req, State};
from_json(Req, State = #{operation := bulk, body := List}) ->
    lists:foreach(
        fun(#{vertex := Vertex, event := Event}) ->
            ok = inbox:post(Vertex, Event)
        end, List),
    {true, Req, State}.

%% @todo
%% GET handler
to_json(Req, State = #{operation := stats}) ->
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

-spec parse_event(binary()) -> {ok, event()} | {error, any()}.
parse_event(Bin) ->
    Event = gmm_utils:decode(Bin),
    case validate_event(Event) of
        ok -> {ok, maps:put(<<"id">>, gmm_utils:uuid(), Event)};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_bulk_events(binary()) -> {ok, list(#{vertex := binary(), event := event()})} | {error, any()}.
parse_bulk_events(Bin) ->
    case gmm_utils:decode(Bin) of
        #{<<"messages">> := List} when is_list(List) ->
            Parser =
                fun
                    (#{<<"vn">> := VertexName, <<"e">> := Event}) when is_binary(VertexName) ->
                        case validate_event(Event) of
                            ok -> #{
                                vertex => gmm_utils:create_vertex_id(VertexName),
                                event => maps:put(<<"id">>, gmm_utils:uuid(), Event)
                            };
                            {error, R} -> {error, R}
                        end;
                    (_) -> {error, "Invalid JSON"}
                end,
            lists:foldl(
                fun
                    (_, {error, R}) -> {error, R};
                    ({error, R}, _) -> {error, R};
                    ({ok, Elem}, {ok, Acc}) -> {ok, [Elem | Acc]}
                end,
                {ok, []}, lists:map(Parser, List));
        _ -> {error, "Invalid JSON"}
    end.

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
        false -> {error, "One of event's parameters is invalid"}
    end;
validate_event(_) ->
    {error, "Event's JSON in a wrong format"}.
