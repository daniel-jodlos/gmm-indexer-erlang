%%%-------------------------------------------------------------------
%%% @doc
%%%  Auxiliary functions for http client
%%% @end
%%%-------------------------------------------------------------------
-module(http_utils).
-author("pawel").

%% API
-export([
    get_address/1,
    build_url/2,
    build_url/3
]).

%%%---------------------------
%% Implementations
%%%---------------------------

%% Router

-spec get_address(Zone :: binary()) -> {ok, binary()} | {error, any()}.
get_address(Zone) ->
    case settings:get_local_tests() of
        true -> {ok, <<Zone/binary, ":", (get_port(Zone))/binary>>};
        false -> {ok, Zone}
    end.

get_port(Zone) ->
    [<<>>, IdxBin] = binary:split(Zone, <<"zone">>),
    Port = 8080 + binary_to_integer(IdxBin),
    integer_to_binary(Port).

%% URL builder

-spec build_url(Address :: binary(), Path :: binary()) -> binary().
build_url(Address, Path) ->
    binary:replace(<< Address/binary, "/", Path/binary >>, <<" ">>, <<"+">>, [global]).

-spec build_url(Address :: binary(), Path :: binary(), Params :: list({binary(), binary()})) -> binary().
build_url(Address, Path, []) ->
    build_url(Address, Path);
build_url(Address, Path, [{FirstPar, FirstVal} | Rest]) ->
    Base = << Address/binary, "/", Path/binary, "?", FirstPar/binary, "=", FirstVal/binary >>,
    Url = lists:foldl(
        fun({Param, Value}, Acc) -> << Acc/binary, "&", Param/binary, "=", Value/binary >> end,
        Base, Rest),
    binary:replace(Url, <<" ">>, <<"+">>, [global]).
