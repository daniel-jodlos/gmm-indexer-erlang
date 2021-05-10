%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2021 22:05
%%%-------------------------------------------------------------------
-module(gmm_http_server).
-author("pawel").

-include("records.hrl").

%% API
-export([start_server/0]).

start_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/users[/:id]", gmm_rest_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
