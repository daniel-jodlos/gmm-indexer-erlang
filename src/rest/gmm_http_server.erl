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

            % returns void
            % POST {name :: string, type :: string} <- params
            {"/graph/vertices", gmm_rest_handler, #{handler => rest_vertices}},

            % returns void
            % POST {bulkRequest :: BulkVertexCreationRequestDto} <-
            {"/graph/vertices/bulk", gmm_rest_handler, #{handler => unimplemented}},

            %%% edges

            % returns void
            % POST {from, to, permissions[, trace], successive} <- params
            {"/graph/edges", gmm_rest_handler, #{handler => rest_edges, op => post}},

            % returns void
            % POST {bulkRequest :: BulkEdgeCreationRequestDto} <- body
            {"/graph/edges/bulk", gmm_rest_handler, #{handler => unimplemented}},
            % returns void
            % POST {from, to, permissions[, trace], successive} <- params
            {"/graph/edges/permissions", gmm_rest_handler, #{handler => rest_edges, op => set_permissions}},
            % returns void
            % POST {from, to[, trace], successive} <- params
            {"/graph/edges/delete", gmm_rest_handler, #{handler => rest_edges, op => delete}},

            %%% basic queries

            % returns boolean
            % POST {from, to} <- params
            {"/is_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            % returns List<String>
            % POST {of} <- params
            {"/list_adjacent", gmm_rest_handler, #{handler => unimplemented}},
            % returns List<String>
            % POST {of} <- params
            {"/list_adjacent_reversed", gmm_rest_handler, #{handler => unimplemented}},
            % returns String
            % POST {from, to} <- params
            {"/permissions", gmm_rest_handler, #{handler => unimplemented}},

            %%% index of some vertex

            % returns List<IndexDto>
            % GET {vertices :: list(String)} <- params
            {"/index", gmm_rest_handler, #{handler => unimplemented}},

            %%% load simulation

            % returns void
            % POST {request :: LoadSimulationRequestDto} <- body
            {"/simulate_load", gmm_rest_handler, #{handler => unimplemented}},

            %%% status checking, settings etc

            % returns void
            % GET {}
            {"/healthcheck", gmm_rest_handler, #{handler => unimplemented}},
            % returns boolean
            % GET {}
            {"/index_ready", gmm_rest_handler, #{handler => unimplemented}},
            % returns DependentZonesDto
            % POST {exclude :: list(ZoneId)} <- body
            {"/dependent_zones", gmm_rest_handler, #{handler => unimplemented}},
            % returns boolean, returns void
            % GET {}, PUT {enabled :: boolean} <- params
            {"/instrumentation", gmm_rest_handler, #{handler => unimplemented}},
            % returns void
            % PUT {enabled :: boolean} <- params
            {"/indexation", gmm_rest_handler, #{handler => unimplemented}},

            %%% naive queries

            % returns ReachesResponseDto
            % POST {from, to}
            {"/naive/reaches", gmm_rest_handler, #{handler => rest_naive, op => reaches}},
            % returns MembersResponseDto
            % POST {of}
            {"/naive/members", gmm_rest_handler, #{handler => rest_naive, op => members}},
            % return EffectivePermissionsResponseDto
            % POST {from, to}
            {"/naive/effective_permissions", gmm_rest_handler, #{handler => rest_naive, op => effective_permissions}},

            %%% indexed queries

            % returns ReachesResponseDto
            % POST {from, to}
            {"/indexed/reaches", gmm_rest_handler, #{handler => unimplemented}},
            % returns MembersResponseDto
            % POST {of}
            {"/indexed/members", gmm_rest_handler, #{handler => unimplemented}},
            % returns EffectivePermissionsResponseDto
            % POST {from, to}
            {"/indexed/effective_permissions", gmm_rest_handler, #{handler => unimplemented}},

            %%% events propagation

            % returns void
            % POST {id, event}
            {"/events", gmm_rest_handler, #{handler => unimplemented}},
            % returns void
            % POST {messages :: BulkMessagesDto}
            {"/events/bulk", gmm_rest_handler, #{handler => unimplemented}},
            % returns EventStats
            % GET {}
            {"/events/stats", gmm_rest_handler, #{handler => unimplemented}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
