%%%-------------------------------------------------------------------
%% @doc
%%  Basic functions to switch between JSON and erlang types
%% @end
%%%-------------------------------------------------------------------

-module(client_utils).

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

    create_vertex_id/2,
    split_vertex_id/1,
    owner_of/1,

    create_edge_id/2,
    split_edge_id/1,

    lists_equal/2
]).


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

%% Vertices

-spec create_vertex_id(Zone :: binary(), Name :: binary()) -> binary().
create_vertex_id(Zone, Name) ->
    <<Zone/binary, "/", Name/binary>>.

-spec split_vertex_id(Bin :: binary()) -> {binary(), binary()}.
split_vertex_id(Bin) ->
    case split_bin(Bin) of
        [Zone, Name] -> {Zone, Name}
    end.

-spec owner_of(Vertex :: binary()) -> binary().
owner_of(Vertex) ->
    case split_vertex_id(Vertex) of
        {Zone, _} -> Zone
    end.

%% Edges

-spec create_edge_id(From :: binary(), To :: binary()) -> binary().
create_edge_id(From, To) -> <<"edge/", From/binary, "/", To/binary>>.

-spec split_edge_id(Bin :: binary()) -> {binary(), binary()}.
split_edge_id(Bin) ->
    case split_bin(Bin) of
        [<<"edge">>, Zone1, Name1, Zone2, Name2] ->
            {create_vertex_id(Zone1, Name1), create_vertex_id(Zone2, Name2)}
    end.


%%%---------------------------
%% More complex functions
%%%---------------------------

-spec lists_equal(L1 :: list(), L1 :: list()) -> boolean().
lists_equal(L1, L2) ->
    (length(L1) == length(L2)) and (lists:sort(L1) -- lists:sort(L2) == []).
