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

    permissions_and/2,
    permissions_or/2,

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


uuid() ->
    list_to_binary(uuid:to_string( uuid:uuid4() )).
