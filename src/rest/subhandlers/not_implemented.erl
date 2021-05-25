%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2021 21:27
%%%-------------------------------------------------------------------
-module(not_implemented).
-author("pawel").

%% API
-export([init/2]).

init(_Req, _State) ->
    erlang:error("Rest handler for this path is not implemented yet").
