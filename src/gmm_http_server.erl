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

%% API
-export([start_server/0]).

start_server() ->
    Dispatch = cowboy_router:compile([
        %% vertices
%%        {'_', [{"/graph/vertices", gmm_rest_handler_vertices, #{}}]},
        {'_', [{"/graph/vertices", gmm_rest_handler, #{handler => rest_handler_vertices}}]},

        %% edges
%%        {'_', [{"/graph/edges", gmm_rest_handler_edges, #{}}]}
        {'_', [{"/graph/edges", gmm_rest_handler, #{handler => rest_handler_edges}}]}

    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
