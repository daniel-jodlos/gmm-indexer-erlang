%%%-------------------------------------------------------------------
%% @doc
%%  @todo
%%  Implements API for handling events sent by other zones
%% @end
%%%-------------------------------------------------------------------

-module(rest_events).
-behavior(cowboy_handler).

%% API
-export([
    init/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.
