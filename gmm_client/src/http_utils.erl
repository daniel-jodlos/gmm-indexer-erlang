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
get_address(<<"zone0">>) ->
    {ok, <<"127.0.0.1:8080">>};
get_address(<<"zone1">>) ->
    {ok, <<"127.0.0.1:8081">>};
get_address(<<"zone2">>) ->
    {ok, <<"127.0.0.1:8082">>};
get_address(_) ->
    {error, "Zone not found"}.

%% URL builder

-spec build_url(Address :: binary(), Path :: binary()) -> binary().
build_url(Address, Path) ->
    NewPath = edoc_lib:escape_uri(Path),
    << Address/binary, "/", NewPath/binary >>.

-spec build_url(Address :: binary(), Path :: binary(), Params :: list({binary(), binary()})) -> binary().
build_url(Address, Path, []) ->
    build_url(Address, Path);
build_url(Address, Path, [{FirstPar, FirstVal} | Rest]) ->
    Base = << Path/binary, "?", FirstPar/binary, "=", FirstVal/binary >>,
    Url = lists:foldl(
        fun({Param, Value}, Acc) -> << Acc/binary, "&", Param/binary, "=", Value/binary >> end,
        Base, Rest),
    NewBase = edoc_lib:escape_uri(Url),
    <<Address/binary, "/", NewBase/binary>>.
