%%%-------------------------------------------------------------------
%%% @doc
%%%  Module to parse data between different types
%%% @end
%%%-------------------------------------------------------------------
-module(parser).
-author("pawel").

%% API
-export([
    parse_boolean/1,
    convert_microseconds_to_iso_8601/1,
    nanosecond_timestamp_to_iso6801/1,
    parse_rest_params/4,
    parse_rest_body/3
]).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

-spec parse_boolean(binary()) -> {ok, boolean()} | {error, any()}.
parse_boolean(<<"1">>) -> {ok, true};
parse_boolean(<<"0">>) -> {ok, false};
parse_boolean(<<"true">>) -> {ok, true};
parse_boolean(<<"false">>) -> {ok, false};
parse_boolean(X) -> {error, {not_a_binary_bool, X}}.


%% Assumption: time to parse is smaller than 1 day, or rather: result of this function is time modulo 24 hours
-spec convert_microseconds_to_iso_8601(integer()) -> binary().
convert_microseconds_to_iso_8601(TotalMicroseconds) when TotalMicroseconds >= 0 ->
    %% decompose TotalMicroseconds
    Microseconds = TotalMicroseconds rem 1000000,
    TotalSeconds = TotalMicroseconds div 1000000,
    Seconds = TotalSeconds rem 60,
    TotalMinutes = TotalSeconds div 60,
    Minutes = TotalMinutes rem 60,
    TotalHours = TotalMinutes div 60,
    Hours = TotalHours rem 24,

    %% parse it to string
    Components = [
        "PT",
        case Hours of
            0 -> "";
            _ -> io_lib:format("~wH", [Hours])
        end,
        case Minutes of
            0 -> "";
            _ -> io_lib:format("~wM", [Minutes])
        end,
        io_lib:format("~.6fS", [(Seconds * 1000000 + Microseconds) / 1000000])
    ],

    %% compose final result
    DurationString = lists:foldl(fun (S, Acc) -> Acc ++ S end, "", Components),
    list_to_binary(DurationString).

-spec nanosecond_timestamp_to_iso6801(integer()) -> binary().
nanosecond_timestamp_to_iso6801(Timestamp) when is_integer(Timestamp), Timestamp > 0 ->
    Nanos = Timestamp rem 1000000000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:system_time_to_universal_time(Timestamp, nanosecond),

    Components = [
        io_lib:format("~4..0w", [Year]),
        "-",
        io_lib:format("~2..0w", [Month]),
        "-",
        io_lib:format("~2..0w", [Day]),
        "T",
        io_lib:format("~2..0w", [Hour]),
        ":",
        io_lib:format("~2..0w", [Minute]),
        ":",
        io_lib:format("~2..0w", [Second]),
        ".",
        io_lib:format("~9..0w", [Nanos]),
        "Z"
    ],

    InstantString = lists:foldl(fun(Elem, Acc) -> Acc ++ Elem end, "", Components),
    list_to_binary(InstantString).


-spec parse_rest_params( Req :: cowboy_req:req(), State :: rest_handler_state(), ParamsSpec :: cowboy:fields(),
    ParsingSpec :: list({atom(), fun((binary()) -> ok | {ok, any()})} ) ) -> rest_handler_state().
parse_rest_params(Req, State, ParamsSpec, ParsingSpec) ->
    try
        ReadParams = cowboy_req:match_qs(ParamsSpec, Req),
        ParsedParams =
            lists:foldl(
                fun({Key, Fun}, Acc) ->
                    case Fun(maps:get(Key, Acc)) of
                        ok -> Acc;
                        {ok, Value} -> maps:update(Key, Value, Acc)
                    end
                end,
                ReadParams, ParsingSpec
            ),
        maps:merge(State, ParsedParams)
    catch Class:Pattern:Stacktrace ->
        gmm_logger:log_error(Class, Pattern, Stacktrace),
        bad_request end.

-spec parse_rest_body( Req :: cowboy_req:req(), State :: rest_handler_state(),
    Parser :: fun((binary()) -> {ok, any()}) ) -> {cowboy_req:req(), rest_handler_state()}.
parse_rest_body(Req, bad_request, _) ->
    {Req, bad_request};
parse_rest_body(Req0, State, Parser) ->
    try
        {ok, Data, Req1} = cowboy_req:read_body(Req0),
        {ok, Body} = Parser( Data ),
        {Req1, maps:merge(State, #{body => Body})}
    catch Class:Pattern:Stacktrace ->
        gmm_logger:log_error(Class, Pattern, Stacktrace),
        {Req0, bad_request} end.
