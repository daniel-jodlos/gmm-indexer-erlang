-module(indexed_queries_using_java_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).
-export([
    operations_test/1
]).

groups() -> [{coverage, [sequence], [
    operations_test
]}].

all() -> [{group, coverage}].

init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

end_per_suite(_Config) ->
    ok = application:stop(gmm_client).

init_per_group(coverage, _Config) ->
    timer:sleep(5000), %% for unknown reasons some delay is needed before executing first tests -
                       %% without it tests fail with reason {badmatch, {error, closed}}
                       %% <<PAWEL>>: It seems that server is not ready immediately; {error, close} indicates,
                       %%            that hackney received error from tcp port
    {ok, _} = application:ensure_all_started(gmm_client),
    ok;
init_per_group(_Group, _Config) ->
    ok.

end_per_group(_Group, _Config) ->
    ok.

operations_test(Config) ->
    % when
    Vertices = test_utils:vertices_from_file(filename:join([?config(data_dir, Config), "graph.json"])),
    Edges = test_utils:edges_from_file(filename:join([?config(data_dir, Config), "graph.json"])),

    lists:foreach(
        fun ([From, To]) ->
            ?assertEqual({ok, true}, client:edge_exists(From, To))
        end,
        Edges
    ),

    % then
    %test_utils:random_operations(members, Vertices, 10),
    test_utils:random_operations(reaches, Vertices, 10),
    test_utils:random_operations(ep, Vertices, 10).
