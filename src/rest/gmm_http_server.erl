%%%-------------------------------------------------------------------
%% @doc
%%  Initializes cowboy server:
%%   compiles available paths, specifies handler modules and so on
%% @end
%%%-------------------------------------------------------------------

-module(gmm_http_server).

%% API
-export([start_server/0]).

start_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            %%% @todo rest_vertices - implemented all except '/bulk'

            %  POST {name, type} -> void
            %  *  GET {} -> map(string->list(string)) (LISTING)
            %  *  GET {id} -> map()
            %  *  DELETE {id} -> void
            {"/graph/vertices", rest_vertices, #{}},

            %  POST {bulkRequest :: BulkVertexCreationRequestDto} -> void
            {"/graph/vertices/bulk", not_implemented, #{}},

            %%% @todo rest_edges - implemented all except '/bulk'

            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges", rest_edges, #{op => add}},
            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges/permissions", rest_edges, #{op => permissions}},
            %  POST {from, to[, trace], successive} -> void
            {"/graph/edges/delete", rest_edges, #{op => delete}},

            %  POST {BODY->bulkRequest :: BulkEdgeCreationRequestDto} -> void
            {"/graph/edges/bulk", not_implemented, #{}},

            %%% @todo rest_basic_queries

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

            %%% @todo rest_meta_info - rest api ready, logic not implemented

            %  GET {} -> void
            {"/healthcheck", rest_meta_info, #{op => health_check}},
            %  GET {} -> boolean
            {"/index_ready", rest_meta_info, #{op => index_ready}},
            %  POST {BODY->exclude :: list(ZoneId)} -> DependentZonesDto
            {"/dependent_zones", rest_meta_info, #{op => dependent_zones}},
            %  GET {} -> boolean
            %  PUT {enabled :: boolean} -> void
            {"/instrumentation", rest_meta_info, #{op => instrumentation}},
            %  PUT {enabled :: boolean} -> void
            {"/indexation", rest_meta_info, #{op => indexation}},

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
