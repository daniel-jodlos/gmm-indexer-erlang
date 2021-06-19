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
    create_vertex_id/1,
    create_vertex_id/2,
    split_bin/2,
    split_bin/1,
    parse_boolean/1,
    convert_microseconds_to_iso_8601/1,
    permissions_and/2,
    permissions_or/2
]).

-include("records.hrl").

%%%---------------------------
%% Implementations
%%%---------------------------

%% JSON manipulation
-spec empty_json() -> binary().
empty_json() -> jiffy:encode({[]}).

-spec encode(any()) -> binary().
encode(Val) -> jiffy:encode(Val).

-spec decode(binary()) -> any().
decode(Val) -> jiffy:decode(Val, [return_maps]).

%% Binary string manipulation
-spec create_vertex_id(Name :: binary()) -> binary().
create_vertex_id(Name) ->
    create_vertex_id(Name, list_to_binary(?ZONE_ID)).

-spec create_vertex_id(Name :: binary(), Zone :: binary()) -> binary().
create_vertex_id(Name, Zone) ->
    <<Zone/binary, "/", Name/binary>>.

-spec split_bin(Bin :: binary(), Delimiter :: binary()) -> {ok, list(binary())} | {error, any()}.
split_bin(Bin, Delimiter) ->
    try
        {ok, binary:split(Bin, Delimiter, [global])}
    catch _:_ ->
        {error, not_a_bin}
    end.

-spec split_bin(Bin :: binary()) -> {ok, list(binary())} | {error, any()}.
split_bin(Bin) ->
    %% default delimiter is '/'
    split_bin(Bin, <<"/">>).

%% Other functions
-spec parse_boolean(binary()) -> {ok, boolean()} | {error, any()}.
parse_boolean(Bin) ->
    try
        case binary_to_atom(Bin) of
            Bool when is_boolean(Bool) -> {ok, Bool};
            _ -> {error, not_a_bool}
        end
    catch
        _:_ -> {error, not_a_bool}
    end.

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

char_to_boolean($0) -> false;
char_to_boolean($1) -> true.

boolean_to_char(true) -> $1;
boolean_to_char(false) -> $0.

-type permissions() :: <<_:5*8>>.
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
