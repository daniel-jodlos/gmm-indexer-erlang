%%%-------------------------------------------------------------------
%% @doc
%%  gmm public API
%% @end
%%%-------------------------------------------------------------------

-module(gmm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gmm_http_server:start_server(),
    gmm_sup:start_link().

stop(_State) -> ok.

%% internal functions
