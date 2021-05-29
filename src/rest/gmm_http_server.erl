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

            %%%-------------------------------------------------------------------
            %%  rest_vertices
            %%  @todo implement '/bulk'
            %%%-------------------------------------------------------------------

            %  POST {name, type} -> void
            %  *  GET {} -> map(string->list(string)) (LISTING)
            %  *  GET {id} -> map()
            %  *  DELETE {id} -> void
            {"/graph/vertices", rest_vertices, #{}},

            %  POST {bulkRequest :: BulkVertexCreationRequestDto} -> void
            {"/graph/vertices/bulk", not_implemented, #{}},

            %%%-------------------------------------------------------------------
            %%  rest_edges
            %%  @todo implement '/bulk'
            %%%-------------------------------------------------------------------

            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges", rest_edges, #{operation => add}},
            %  POST {from, to, permissions[, trace], successive} -> void
            {"/graph/edges/permissions", rest_edges, #{operation => permissions}},
            %  POST {from, to[, trace], successive} -> void
            {"/graph/edges/delete", rest_edges, #{operation => delete}},

            %  POST {BODY->bulkRequest :: BulkEdgeCreationRequestDto} -> void
            {"/graph/edges/bulk", not_implemented, #{}},

            %%%-------------------------------------------------------------------
            %% rest_basic_queries
            %%%-------------------------------------------------------------------

            %  POST {from, to} -> boolean
            {"/is_adjacent", rest_basic_queries, #{operation => is_adjacent}},
            %  POST {of} -> List<String>(CHILDREN)
            {"/list_adjacent", rest_basic_queries, #{operation => list_adjacent}},
            %  POST {of} -> List<String>(PARENTS)
            {"/list_adjacent_reversed", rest_basic_queries, #{operation => list_adjacent_reversed}},
            %  POST {from, to} -> String
            {"/permissions", rest_basic_queries, #{operation => permissions}},

            %%%-------------------------------------------------------------------
            %%  rest_graph_index
            %%  @todo implement api+logic
            %%%-------------------------------------------------------------------

            %  GET {vertices :: list(String)} -> List<IndexDto>
            {"/index", not_implemented, #{}},

            %%%-------------------------------------------------------------------
            %%  rest_load_simulator
            %%  @todo implement logic
            %%%-------------------------------------------------------------------

            %  POST {BODY->request :: LoadSimulationRequestDto} -> void
            {"/simulate_load", rest_load_simulator, #{}},

            %%%-------------------------------------------------------------------
            %%  rest_meta_info
            %%  @todo implement logic
            %%%-------------------------------------------------------------------

            %  GET {} -> void
            {"/healthcheck", rest_meta_info, #{operation => health_check}},
            %  GET {} -> boolean
            {"/index_ready", rest_meta_info, #{operation => index_ready}},
            %  POST {BODY->exclude :: list(ZoneId)} -> DependentZonesDto
            {"/dependent_zones", rest_meta_info, #{operation => dependent_zones}},
            %  GET {} -> boolean
            %  PUT {enabled :: boolean} -> void
            {"/instrumentation", rest_meta_info, #{operation => instrumentation}},
            %  PUT {enabled :: boolean} -> void
            {"/indexation", rest_meta_info, #{operation => indexation}},

            %%%-------------------------------------------------------------------
            %%  rest_queries  --  algorithm = naive
            %%  @todo implement logic
            %%%-------------------------------------------------------------------

            %  POST {from, to} -> ReachesResponseDto
            {"/naive/reaches", rest_queries, #{operation => reaches, algorithm => naive}},
            %  POST {of} -> MembersResponseDto
            {"/naive/members", rest_queries, #{operation => members, algorithm => naive}},
            %  POST {from, to} -> EffectivePermissionsResponseDto
            {"/naive/effective_permissions", rest_queries, #{operation => effective_permissions, algorithm => naive}},

            %%%-------------------------------------------------------------------
            %%  rest_queries  --  algorithm = indexed
            %%  @todo implement logic
            %%%-------------------------------------------------------------------

            %  POST {from, to} -> ReachesResponseDto
            {"/indexed/reaches", rest_queries, #{operation => reaches, algorithm => indexed}},
            %  POST {of} -> MembersResponseDto
            {"/indexed/members", rest_queries, #{operation => members, algorithm => indexed}},
            %  POST {from, to} -> EffectivePermissionsResponseDto
            {"/indexed/effective_permissions", rest_queries, #{operation => effective_permissions, algorithm => indexed}},

            %%%-------------------------------------------------------------------
            %%  rest_events
            %%  @todo implement api+logic
            %%%-------------------------------------------------------------------

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
