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
        {'_', [
            %% vertices
            {"/graph/vertices", gmm_rest_handler, #{handler => rest_handler_vertices}},

            %% edges
            {"/graph/edges", gmm_rest_handler, #{handler => rest_handler_edges}},

            %%%% @todo implement all those, lol

            %% graph structure
            {"/is_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            {"/list_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            {"/list_adjacent_reversed", gmm_rest_handler, #{handler => unimplemented}},
            {"/permissions", gmm_rest_handler, #{handler => unimplemented}},

            %% indexation settings
            {"/index", gmm_rest_handler, #{handler => unimplemented}},

            %% events propagation
            {"/events", gmm_rest_handler, #{handler => unimplemented}},
            {"/events/bulk", gmm_rest_handler, #{handler => unimplemented}},
            {"/events/stats", gmm_rest_handler, #{handler => unimplemented}},

            %% status checking and stuff
            {"/simulate_load", gmm_rest_handler, #{handler => unimplemented}},
            {"/healthcheck", gmm_rest_handler, #{handler => unimplemented}},
            {"/index_ready", gmm_rest_handler, #{handler => unimplemented}},
            {"/dependent_zones", gmm_rest_handler, #{handler => unimplemented}},
            {"/instrumentation", gmm_rest_handler, #{handler => unimplemented}},

            %% naive queries
            {"/naive/reaches", gmm_rest_handler, #{handler => unimplemented}},
            {"/naive/members", gmm_rest_handler, #{handler => unimplemented}},
            {"/naive/effective_permissions", gmm_rest_handler, #{handler => unimplemented}},

            %% indexed queries
            {"/indexed/reaches", gmm_rest_handler, #{handler => unimplemented}},
            {"/indexed/members", gmm_rest_handler, #{handler => unimplemented}},
            {"/indexed/effective_permissions", gmm_rest_handler, #{handler => unimplemented}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
