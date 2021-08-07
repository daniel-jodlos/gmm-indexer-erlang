-module(indexed_queries_SUITE).
-include_lib("common_test/include/ct.hrl").

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
    [Vertices, _Edges] = test_utils:load_graph(filename:join([?config(data_dir, Config), "graph.json"])),

    % then
    test_utils:random_operations(member, Vertices, 10),
    test_utils:random_operations(reaches, Vertices, 10),
    test_utils:random_operations(ep, Vertices, 10).
