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
            %%%% @todo implement all those, lol

            %%% vertices

            % POST {name, type}
            {"/graph/vertices", gmm_rest_handler, #{handler => rest_handler_vertices}},

            % POST {bulkRequest :: BulkVertexCreationRequestDto}
            {"/graph/vertices/bulk", gmm_rest_handler, #{handler => unimplemented}},

            %%% edges

            % POST {from, to, permissions[, trace], successive}
            {"/graph/edges", gmm_rest_handler, #{handler => rest_handler_edges}},

            % POST {bulkRequest :: BulkEdgeCreationRequestDto}
            {"/graph/edges/bulk", gmm_rest_handler, #{handler => unimplemented}},
            % POST {from, to, permissions[, trace], successive}
            {"/graph/edges/permissions", gmm_rest_handler, #{handler => unimplemented}},
            % POST {from, to[, trace], successive}
            {"/graph/edges/delete", gmm_rest_handler, #{handler => unimplemented}},

            %%% basic queries

            % POST {from, to}
            {"/is_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            % POST {of}
            {"/list_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            % POST {of}
            {"/list_adjacent_reversed", gmm_rest_handler, #{handler => unimplemented}},
            % POST {from, to}
            {"/permissions", gmm_rest_handler, #{handler => unimplemented}},

            %%% index of some vertex

            % GET {vertices :: list(String)}
            {"/index", gmm_rest_handler, #{handler => unimplemented}},

            %%% load simulation

            % POST {request :: LoadSimulationRequestDto}
            {"/simulate_load", gmm_rest_handler, #{handler => unimplemented}},

            %%% status checking, settings etc

            % GET {}
            {"/healthcheck", gmm_rest_handler, #{handler => unimplemented}},
            % GET {}
            {"/index_ready", gmm_rest_handler, #{handler => unimplemented}},
            % POST {exclude :: list(ZoneId)}
            {"/dependent_zones", gmm_rest_handler, #{handler => unimplemented}},
            % GET {}, PUT {enabled :: boolean}
            {"/instrumentation", gmm_rest_handler, #{handler => unimplemented}},
            % PUT {enabled :: boolean}
            {"/indexation", gmm_rest_handler, #{handler => unimplemented}},

            %%% naive queries

            % POST {from, to}
            {"/naive/reaches", gmm_rest_handler, #{handler => unimplemented}},
            % POST {of}
            {"/naive/members", gmm_rest_handler, #{handler => unimplemented}},
            % POST {from, to}
            {"/naive/effective_permissions", gmm_rest_handler, #{handler => unimplemented}},

            %%% indexed queries

            % POST {from, to}
            {"/indexed/reaches", gmm_rest_handler, #{handler => unimplemented}},
            % POST {of}
            {"/indexed/members", gmm_rest_handler, #{handler => unimplemented}},
            % POST {from, to}
            {"/indexed/effective_permissions", gmm_rest_handler, #{handler => unimplemented}},

            %%% events propagation

            % POST {id, event}
            {"/events", gmm_rest_handler, #{handler => unimplemented}},
            % POST {messages :: BulkMessagesDto}
            {"/events/bulk", gmm_rest_handler, #{handler => unimplemented}},
            % GET {}
            {"/events/stats", gmm_rest_handler, #{handler => unimplemented}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
