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
            %%% @todo rest_vertices - mostly implemented

            %  POST {name, type} -> void
            %  *  GET {} -> map(string->list(string)) (LISTING)
            %  *  GET {id} -> map()
            %  *  DELETE {id} -> void
            {"/graph/vertices", rest_vertices, #{}},

            %  POST {bulkRequest :: BulkVertexCreationRequestDto} -> void
            {"/graph/vertices/bulk", not_implemented, #{}},

            %%% @todo rest_edges - mostly implemented

            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges", rest_edges, #{op => add}},
            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges/permissions", rest_edges, #{op => permissions}},
            %  POST {from, to[, trace], successive} -> void
            {"/graph/edges/delete", rest_edges, #{op => delete}},

            %  POST {BODY->bulkRequest :: BulkEdgeCreationRequestDto} -> void
            {"/graph/edges/bulk", not_implemented, #{}},

            %%% @todo rest_basic_queries - mostly implemented

            %  POST {from, to} -> boolean
            {"/is_adjacent", rest_basic_queries, #{op => is_adjacent}},
            %  POST {of} -> List<String>(CHILDREN)
            {"/list_adjacent", rest_basic_queries, #{op => list_adjacent}},
            %  POST {of} -> List<String>(PARENTS)
            {"/list_adjacent_reversed", rest_basic_queries, #{op => list_adjacent_reversed}},
            %  POST {from, to} -> String
            {"/permissions", rest_basic_queries, #{op => permissions}},

            %%% @todo rest_graph_index

            %  GET {vertices :: list(String)} -> List<IndexDto>
            {"/index", not_implemented, #{}},

            %%% @todo rest_load_simulator

            %  POST {BODY->request :: LoadSimulationRequestDto} -> void
            {"/simulate_load", not_implemented, #{}},

            %%% @todo rest_meta_info - partially implemented

            %  GET {} -> void
            {"/healthcheck", rest_meta_info, #{op => health_check}},
            %  GET {} -> boolean
            {"/index_ready", not_implemented, #{op => index_ready}},
            %% @todo implement and then change handler below to rest_meta_info
            %  POST {BODY->exclude :: list(ZoneId)} -> DependentZonesDto
            {"/dependent_zones", not_implemented, #{op => dependent_zones}},
            %  GET {} -> boolean
            %  PUT {enabled :: boolean} -> void
            {"/instrumentation", not_implemented, #{op => instrumentation}},
            %  PUT {enabled :: boolean} -> void
            {"/indexation", not_implemented, #{op => indexation}},

            %%% @todo rest_queries_naive

            %  POST {from, to} -> ReachesResponseDto
            {"/naive/reaches", not_implemented, #{op => reaches}},
            %  POST {of} -> MembersResponseDto
            {"/naive/members", not_implemented, #{op => members}},
            %  POST {from, to} -> EffectivePermissionsResponseDto
            {"/naive/effective_permissions", not_implemented, #{op => effective_permissions}},

            %%% @todo rest_queries_indexed

            %  POST {from, to} -> ReachesResponseDto
            {"/indexed/reaches", not_implemented, #{}},
            %  POST {of} -> MembersResponseDto
            {"/indexed/members", not_implemented, #{}},
            %  POST {from, to} -> EffectivePermissionsResponseDto
            {"/indexed/effective_permissions", not_implemented, #{}},

            %%% @todo rest_events

            %  POST {id, BODY->event} -> void
            {"/events", not_implemented, #{}},
            %  POST {BODY->messages :: BulkMessagesDto} -> void
            {"/events/bulk", not_implemented, #{}},
            %  GET {} -> EventStats
            {"/events/stats", not_implemented, #{}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, os:getenv("PORT", 8080)}],
        #{env => #{dispatch => Dispatch}}
    ).
