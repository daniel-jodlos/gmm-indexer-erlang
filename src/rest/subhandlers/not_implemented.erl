%%%-------------------------------------------------------------------
%% @doc
%%  Empty handler module
%%  It's only function is to throw proper error in server console
%%
%%  It will be removed once all handlers are implemented
%% @end
%%%-------------------------------------------------------------------

-module(not_implemented).
-behavior(cowboy_handler).

%% API
-export([init/2]).

init(_Req, _State) ->
    erlang:error("Rest handler for this path is not implemented yet").
