%%%-------------------------------------------------------------------
%% @doc
%%  Basic functions to switch between JSON and erlang types
%% @end
%%%-------------------------------------------------------------------

-module(gmm_utils).

%% API
-export([
    empty_json/0,
    encode/1,
    decode/1,

    parse_boolean/1,
    validate_vertex_id/1,
    validate_edge_id/1,

    split_bin/2,
    split_bin/1,

    zone_id/0,
    create_vertex_id/2,
    create_vertex_id/1,
    split_vertex_id/1,
    owner_of/1,

    create_edge_id/2,
    split_edge_id/1,

    list_other_zones/0,
    batch_size/0,

    get_instrumentation_enabled/0,
    set_instrumentation_enabled/1,
    get_indexation_enabled/0,
    set_indexation_enabled/1,

    convert_microseconds_to_iso_8601/1,
    nanosecond_timestamp_to_iso6801/1,

    permissions_and/2,
    permissions_or/2,

    parse_rest_params/4,
    parse_rest_body/3,

    log_error/3,
    log_cowboy_req/2,
    uuid/0
]).

-include("records.hrl").


%%%---------------------------
%% JSON manipulation
%%%---------------------------

-spec empty_json() -> binary().
empty_json() -> jiffy:encode({[]}).

-spec encode(any()) -> binary().
encode(Val) -> jiffy:encode(Val).

-spec decode(binary()) -> any().
decode(Val) -> jiffy:decode(Val, [return_maps]).


%%%---------------------------
%% Argument's validation
%%%---------------------------

-spec parse_boolean(binary()) -> {ok, boolean()} | {error, any()}.
parse_boolean(<<"true">>) -> {ok, true};
parse_boolean(<<"false">>) -> {ok, false};
parse_boolean(X) -> {error, {not_a_binary_bool, X}}.

-spec validate_vertex_id(Bin :: binary()) -> ok | {error, any()}.
validate_vertex_id(Bin) when is_binary(Bin) ->
    try
        case split_vertex_id(Bin) of
            {Zone, Name} when byte_size(Zone) > 0, byte_size(Name) > 0 -> ok;
            _ -> {error, {invalid_vertex_format, Bin}}
        end
    catch _:_ -> {error, {could_not_split, Bin}} end;
validate_vertex_id(X) ->
    {error, {not_a_bin, X}}.

-spec validate_edge_id(Bin :: binary()) -> ok | {error, any()}.
validate_edge_id(Bin) when is_binary(Bin) ->
    try
        case split_edge_id(Bin) of
            {From, To} when byte_size(From) > 0, byte_size(To) > 0 ->
                ok = validate_vertex_id(From),
                ok = validate_vertex_id(To),
                ok;
            _ -> {error, {invalid_edge, {Bin}}}
        end
    catch _:_ -> {error, {could_not_split, Bin}} end;
validate_edge_id(X) ->
    {error, {not_a_bin, X}}.


%%%---------------------------
%% Binary string manipulation
%%%---------------------------

-spec split_bin(Bin :: binary(), Delimiter :: binary()) -> list(binary()).
split_bin(Bin, Delimiter) when is_binary(Bin), is_binary(Delimiter), byte_size(Delimiter) > 0 ->
    binary:split(Bin, Delimiter, [global]).

-spec split_bin(Bin :: binary()) -> list(binary()).
split_bin(Bin) when is_binary(Bin) ->
    %% default delimiter is '/'
    split_bin(Bin, <<"/">>).


%%%---------------------------
%% Manipulation of IDs
%%%---------------------------

%% Get id of this zone in binary form

-spec zone_id() -> binary().
zone_id() ->
    list_to_binary(?ZONE_ID).

%% Vertices

-spec create_vertex_id(Zone :: binary(), Name :: binary()) -> binary().
create_vertex_id(Zone, Name) -> <<Zone/binary, ":", Name/binary>>.

-spec create_vertex_id(Name :: binary()) -> binary().
create_vertex_id(Name) ->
    create_vertex_id(zone_id(), Name).

-spec split_vertex_id(Bin :: binary()) -> {binary(), binary()}.
split_vertex_id(Bin) ->
    {_, _} = list_to_tuple(split_bin(Bin, <<":">>)).

-spec owner_of(Vertex :: binary()) -> binary().
owner_of(Vertex) ->
    element(1, split_vertex_id(Vertex)).

%% Edges

-spec create_edge_id(From :: binary(), To :: binary()) -> binary().
create_edge_id(From, To) -> <<"edge/", From/binary, "/", To/binary>>.

-spec split_edge_id(Bin :: binary()) -> {binary(), binary()}.
split_edge_id(Bin) ->
    [<<"edge">>, From, To] = split_bin(Bin),
    {From, To}.


%%%---------------------------
%% More complex functions
%%%---------------------------

-spec list_other_zones() -> list(binary()).
list_other_zones() ->
    [<<"zone0">>, <<"zone1">>, <<"zone2">>] -- [gmm_utils:zone_id()].

-spec batch_size() -> integer().
batch_size() -> 5.


-spec get_instrumentation_enabled() -> boolean().
get_instrumentation_enabled() ->
    case os:getenv("INSTRUMENTATION_ENABLED") of
        "true" -> true;
        _ -> false
    end.

-spec set_instrumentation_enabled(boolean()) -> true.
set_instrumentation_enabled(NewVal) when is_boolean(NewVal) ->
    os:putenv("INSTRUMENTATION_ENABLED", atom_to_list(NewVal)).

-spec get_indexation_enabled() -> boolean().
get_indexation_enabled() ->
    case os:getenv("INDEXATION_ENABLED") of
        "true" -> true;
        _ -> false
    end.

-spec set_indexation_enabled(boolean()) -> true.
set_indexation_enabled(NewVal) when is_boolean(NewVal) ->
    os:putenv("INDEXATION_ENABLED", atom_to_list(NewVal)).


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


-spec char_to_boolean(char()) -> boolean().
char_to_boolean($0) -> false;
char_to_boolean($1) -> true.

-spec boolean_to_char(boolean()) -> char().
boolean_to_char(true) -> $1;
boolean_to_char(false) -> $0.

-spec combine_permissions(A :: permissions(), B :: permissions(),
    Fun :: fun((permissions(), permissions()) -> permissions())) -> permissions().
combine_permissions(A, B, Fun) ->
    Alist = lists:map(fun char_to_boolean/1, binary_to_list(A)),
    Blist = lists:map(fun char_to_boolean/1, binary_to_list(B)),
    Clist = lists:map(fun ({X,Y}) -> boolean_to_char(Fun(X,Y)) end, lists:zip(Alist, Blist)),
    list_to_binary(Clist).

-spec permissions_and(A :: permissions(), B :: permissions()) -> permissions().
permissions_and(A,B) ->
    combine_permissions(A,B, fun (X,Y) -> X and Y end).

-spec permissions_or(A :: permissions(), B :: permissions()) -> permissions().
permissions_or(A,B) ->
    combine_permissions(A,B, fun (X,Y) -> X or Y end).


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
        log_error(Class, Pattern, Stacktrace),
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
        log_error(Class, Pattern, Stacktrace),
        {Req0, bad_request} end.

log_error(Class, Pattern, Stacktrace) ->
    io:format("Class: ~p;\nPattern: ~p;\nStacktrace: ~p\n\n", [Class, Pattern, Stacktrace]).

log_cowboy_req(Req, State) ->
    io:format("==============================\n\n"),
    io:format("Req = ~p\n\n", [Req]),
    io:format("------------------------------\n\n"),
    io:format("State = ~p\n\n", [State]),
    io:format("==============================\n\n").


uuid() ->
    list_to_binary(uuid:to_string( uuid:uuid4() )).
