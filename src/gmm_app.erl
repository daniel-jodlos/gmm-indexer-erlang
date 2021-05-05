%%%-------------------------------------------------------------------
%% @doc gmm public API
%% @end
%%%-------------------------------------------------------------------

-module(gmm_app).

-behaviour(application).

-export([start/2, stop/1]).

-record(state, {table = users_table}).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [{"/users[/:id]", gmm_handler, #state{}}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
        ),    
    gmm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions