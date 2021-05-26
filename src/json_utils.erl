%%%-------------------------------------------------------------------
%% @doc
%%  Basic functions to switch between JSON and erlang types
%% @end
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
