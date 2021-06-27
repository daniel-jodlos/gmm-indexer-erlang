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
get_address(<<"zone1">>) ->
    {ok, <<"localhost:8080">>};
get_address(<<"zone2">>) ->
    {ok, <<"localhost:8081">>};
get_address(<<"zone3">>) ->
    {ok, <<"localhost:8082">>};
get_address(_) ->
    {error, "Zone couldn't be located"}.

%% URL builder

-spec build_url(Address :: binary(), Path :: binary()) -> binary().
build_url(Address, Path) ->
    << Address/binary, "/", Path/binary >>.

-spec build_url(Address :: binary(), Path :: binary(), Params :: list({binary(), binary()})) -> binary().
build_url(Address, Path, []) ->
    build_url(Address, Path);
build_url(Address, Path, [{FirstPar, FirstVal} | Rest]) ->
    Base = << Address/binary, "/", Path/binary, "?", FirstPar/binary, "=", FirstVal/binary >>,
    lists:foldl(
        fun({Param, Value}, Acc) -> << Acc/binary, "&", Param/binary, "=", Value/binary >> end,
        Base, Rest).