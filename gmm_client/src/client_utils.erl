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

    bin_to_bool/1,
    validate_vertex_id/1,

    split_bin/2,
    split_bin/1,

    create_vertex_id/2,
    split_vertex_id/1,
    owner_of/1,

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
%% Argument's parsing
%%%---------------------------

-spec bin_to_bool(binary()) -> boolean().
bin_to_bool(<<"true">>) -> true;
bin_to_bool(<<"false">>) -> false.

-spec validate_vertex_id(Bin :: binary()) -> ok | {error, any()}.
validate_vertex_id(Bin) when is_binary(Bin) ->
    try
        case split_bin(Bin, <<":">>) of
            [Zone, Name] when byte_size(Zone) > 0, byte_size(Name) > 0 -> ok;
            _ -> {error, "Invalid format"}
        end
    catch _:_ -> {error, "Couldn't split"} end;
validate_vertex_id(_) ->
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
    <<Zone/binary, ":", Name/binary>>.

-spec split_vertex_id(Bin :: binary()) -> {binary(), binary()}.
split_vertex_id(Bin) ->
    {_, _} = list_to_tuple(split_bin(Bin, <<":">>)).

-spec owner_of(Vertex :: binary()) -> binary().
owner_of(Vertex) ->
    element(1, split_vertex_id(Vertex)).


%%%---------------------------
%% More complex functions
%%%---------------------------

-spec lists_equal(L1 :: list(), L2 :: list()) -> boolean().
lists_equal(L1, L2) ->
    (length(L1) == length(L2)) and (lists:sort(L1) -- lists:sort(L2) == []).
