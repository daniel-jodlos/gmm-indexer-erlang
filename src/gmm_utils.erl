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

    convert_microseconds_to_iso_8601/1
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
parse_boolean(_) -> {error, "Not a bool in binary format"}.

-spec validate_vertex_id(Bin :: binary()) -> ok | {error, any()}.
validate_vertex_id(Bin) when is_binary(Bin) ->
    try
        case split_bin(Bin) of
            [Zone, Name] when byte_size(Zone) > 0, byte_size(Name) > 0 -> ok;
            _ -> {error, "Invalid format"}
        end
    catch _:_ -> {error, "Couldn't split"} end;
validate_vertex_id(_) ->
    {error, "Not a binary"}.

-spec validate_edge_id(Bin :: binary()) -> ok | {error, any()}.
validate_edge_id(Bin) when is_binary(Bin) ->
    try
        case split_bin(Bin) of
            [<<"edge">>, Zone1, Name1, Zone2, Name2] when byte_size(Zone1) > 0,
                byte_size(Name1) > 0, byte_size(Zone2) > 0, byte_size(Name2) > 0 -> ok;
            _ -> {error, "Invalid format"}
        end
    catch _:_ -> {error, "Couldn't split"} end;
validate_edge_id(_) ->
    {error, "Not a binary"}.


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
create_vertex_id(Zone, Name) -> <<Zone/binary, "/", Name/binary>>.

-spec create_vertex_id(Name :: binary()) -> binary().
create_vertex_id(Name) ->
    create_vertex_id(zone_id(), Name).

-spec split_vertex_id(Bin :: binary()) -> {binary(), binary()}.
split_vertex_id(Bin) ->
    {_, _} = list_to_tuple(split_bin(Bin)).

-spec owner_of(Vertex :: binary()) -> binary().
owner_of(Vertex) ->
    element(1, split_vertex_id(Vertex)).

%% Edges

%% Other functions

-spec create_edge_id(From :: binary(), To :: binary()) -> binary().
create_edge_id(From, To) -> <<"edge/", From/binary, "/", To/binary>>.

-spec split_edge_id(Bin :: binary()) -> {binary(), binary()}.
split_edge_id(Bin) ->
    [<<"edge">>, Zone1, Name1, Zone2, Name2] = split_bin(Bin),
    {create_vertex_id(Zone1, Name1), create_vertex_id(Zone2, Name2)}.


%%%---------------------------
%% More complex functions
%%%---------------------------

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
    SecondsString = io_lib:format("~.6fS", [(Seconds * 1000000 + Microseconds) / 1000000]),
    MinutesString =
    case Minutes of
        0 -> "";
        _ -> io_lib:format("~wM", [Minutes])
    end,
    HoursString =
    case Hours of
        0 -> "";
        _ -> io_lib:format("~wH", [Hours])
    end,

    %% compose final result
    TimeString = "PT" ++ HoursString ++ MinutesString ++ SecondsString,
    list_to_binary(TimeString).
