-module(naive_queries_SUITE).
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
    operations_test/1,
    operations_after_graph_update_test/1
]).

groups() -> [{coverage, [sequence], [
    operations_test,
    operations_after_graph_update_test
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

operations_test(_Config) ->
    % when
    client:add_vertex(<<"zone1">>, <<"user">>, <<"User1">>),
    client:add_vertex(<<"zone1">>, <<"user">>, <<"User2">>),
    client:add_vertex(<<"zone2">>, <<"user">>, <<"User3">>),
    client:add_vertex(<<"zone3">>, <<"user">>, <<"User4">>),
    client:add_vertex(<<"zone3">>, <<"user">>, <<"User5">>),
    client:add_vertex(<<"zone1">>, <<"group">>, <<"Group1">>),
    client:add_vertex(<<"zone2">>, <<"group">>, <<"Group2">>),
    client:add_vertex(<<"zone2">>, <<"group">>, <<"Group3">>),
    client:add_vertex(<<"zone3">>, <<"group">>, <<"Group4">>),
    client:add_vertex(<<"zone1">>, <<"space">>, <<"Space1">>),
    client:add_vertex(<<"zone1">>, <<"space">>, <<"Space2">>),
    client:add_vertex(<<"zone2">>, <<"space">>, <<"Space3">>),
    client:add_vertex(<<"zone3">>, <<"space">>, <<"Space4">>),
    client:add_vertex(<<"zone1">>, <<"provider">>, <<"Provider1">>),
    client:add_vertex(<<"zone2">>, <<"provider">>, <<"Provider2">>),
    client:add_vertex(<<"zone3">>, <<"provider">>, <<"Provider3">>),

    client:add_edge(<<"zone1/User1">>, <<"zone1/Group1">>, <<"01110">>, <<"trace1">>),
    client:add_edge(<<"zone1/Group1">>, <<"zone1/Provider1">>, <<"10101">>, <<"trace2">>),
    client:add_edge(<<"zone1/User2">>, <<"zone3/Space4">>, <<"00100">>, <<"trace3">>),
    client:add_edge(<<"zone2/User3">>, <<"zone2/Group3">>, <<"11111">>, <<"trace4">>),
    client:add_edge(<<"zone2/Group3">>, <<"zone2/Group2">>, <<"00010">>, <<"trace5">>),
    client:add_edge(<<"zone2/Group2">>, <<"zone1/Space2">>, <<"10000">>, <<"trace6">>),
    client:add_edge(<<"zone2/Space3">>, <<"zone2/Provider2">>, <<"01100">>, <<"trace7">>),
    client:add_edge(<<"zone3/User4">>, <<"zone2/Group3">>, <<"01000">>, <<"trace8">>),
    client:add_edge(<<"zone2/Group3">>, <<"zone3/Group4">>, <<"10000">>, <<"trace9">>),
    client:add_edge(<<"zone3/User5">>, <<"zone3/Group4">>, <<"01101">>, <<"trace10">>),
    client:add_edge(<<"zone3/Group4">>, <<"zone3/Space4">>, <<"10111">>, <<"trace11">>),
    client:add_edge(<<"zone3/Space4">>, <<"zone2/Provider2">>, <<"11110">>, <<"trace12">>),

    % then
    ok.

operations_after_graph_update_test(_Config) ->
    % when
    client:delete_edge(<<"zone1/User1">>, <<"zone1/Group1">>, <<"trace1">>),
    client:delete_edge(<<"zone2/Group3">>, <<"zone2/Group2">>, <<"trace5">>),
    client:delete_edge(<<"zone2/Group3">>, <<"zone3/Group4">>, <<"trace9">>),
    client:delete_edge(<<"zone3/User5">>, <<"zone3/Group4">>, <<"trace10">>),
    client:delete_edge(<<"zone3/Group4">>, <<"zone3/Space4">>, <<"trace11">>),

    % then
    ok.
