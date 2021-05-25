%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2021 20:58
%%%-------------------------------------------------------------------
-module(json_utils).
-author("pawel").

%% API
-export([
    empty_json/0,
    encode/1,
    decode/1
]).

empty_json() ->
    jiffy:encode({[]}).

encode(Val) ->
    jiffy:encode(Val).

decode(Val) ->
    jiffy:decode(Val, [return_maps]).
