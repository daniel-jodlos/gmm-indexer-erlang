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
    client:add_edge(<<"zone2/User3">>, <<"zone2/Group3">>, <<"00011">>, <<"trace4">>),
    client:add_edge(<<"zone2/Group3">>, <<"zone2/Group2">>, <<"00010">>, <<"trace5">>),
    client:add_edge(<<"zone2/Group2">>, <<"zone1/Space2">>, <<"10000">>, <<"trace6">>),
    client:add_edge(<<"zone2/Space3">>, <<"zone2/Provider2">>, <<"01100">>, <<"trace7">>),
    client:add_edge(<<"zone3/User4">>, <<"zone2/Group3">>, <<"01000">>, <<"trace8">>),
    client:add_edge(<<"zone2/Group3">>, <<"zone3/Group4">>, <<"10000">>, <<"trace9">>),
    client:add_edge(<<"zone3/User5">>, <<"zone3/Group4">>, <<"01101">>, <<"trace10">>),
    client:add_edge(<<"zone3/Group4">>, <<"zone3/Space4">>, <<"10111">>, <<"trace11">>),
    client:add_edge(<<"zone3/Space4">>, <<"zone2/Provider2">>, <<"11110">>, <<"trace12">>),

    % then
    {ok, #{<<"members">> := MembersResponse1}} = client:naive_members(<<"zone3/Space4">>),
    {ok, #{<<"members">> := MembersResponse2}} = client:naive_members(<<"zone2/Group2">>),
    {ok, #{<<"members">> := MembersResponse3}} = client:naive_members(<<"zone1/Space1">>),
    {ok, #{<<"members">> := MembersResponse4}} = client:naive_members(<<"zone1/Provider1">>),

    {ok, #{<<"reaches">> := ReachesResponse1}} = client:naive_reaches(<<"zone2/User3">>, <<"zone3/Provider3">>),
    {ok, #{<<"reaches">> := ReachesResponse2}} = client:naive_reaches(<<"zone3/User5">>, <<"zone3/Space4">>),
    {ok, #{<<"reaches">> := ReachesResponse3}} = client:naive_reaches(<<"zone2/User3">>, <<"zone2/Provider2">>),
    {ok, #{<<"reaches">> := ReachesResponse4}} = client:naive_reaches(<<"zone1/User1">>, <<"zone1/Provider1">>),

    {ok, #{<<"effectivePermissions">> := EPResponse1}} =
        client:naive_effective_permissions(<<"zone1/User1">>, <<"zone1/Provider1">>),
    {ok, #{<<"effectivePermissions">> := EPResponse2}} =
        client:naive_effective_permissions(<<"zone3/User5">>, <<"zone2/Provider2">>),
    {ok, #{<<"effectivePermissions">> := EPResponse3}} =
        client:naive_effective_permissions(<<"zone2/User3">>, <<"zone2/Provider2">>),

    ?assert(client_utils:lists_equal(MembersResponse1, [<<"zone1/User2">>,
        <<"zone2/User3">>, <<"zone2/Group3">>, <<"zone3/User4">>, <<"zone3/User5">>, <<"zone3/Group4">>])),
    ?assert(client_utils:lists_equal(MembersResponse2, [<<"zone2/User3">>, <<"zone2/Group3">>, <<"zone3/User4">>])),
    ?assert(client_utils:lists_equal(MembersResponse3, [])),
    ?assert(client_utils:lists_equal(MembersResponse4, [<<"zone1/User1">>, <<"zone1/Group1">>])),

    ?assertEqual(false, ReachesResponse1),
    ?assertEqual(true, ReachesResponse2),
    ?assertEqual(true, ReachesResponse3),
    ?assertEqual(true, ReachesResponse4),

    ?assertEqual(<<"01110">>, EPResponse1),
    ?assertEqual(<<"01101">>, EPResponse2),
    ?assertEqual(<<"00011">>, EPResponse3).

operations_after_graph_update_test(_Config) ->
    % when
    client:delete_edge(<<"zone2/Group3">>, <<"zone2/Group2">>, <<"trace5">>),
    client:delete_edge(<<"zone3/User5">>, <<"zone3/Group4">>, <<"trace10">>),
    client:set_permissions(<<"zone2/Group3">>, <<"zone3/Group4">>, <<"00010">>, <<"trace9">>),
    client:set_permissions(<<"zone3/Space4">>, <<"zone2/Provider2">>, <<"01010">>, <<"trace12">>),
    client:add_edge(<<"zone2/Group2">>, <<"zone3/Provider3">>, <<"01101">>, <<"trace13">>),
    client:add_edge(<<"zone3/User5">>, <<"zone3/Space3">>, <<"01101">>, <<"trace14">>),

    % then
    {ok, #{<<"members">> := MembersResponse1}} = client:naive_members(<<"zone3/Space4">>),
    {ok, #{<<"members">> := MembersResponse2}} = client:naive_members(<<"zone2/Group2">>),
    {ok, #{<<"members">> := MembersResponse3}} = client:naive_members(<<"zone1/Space1">>),
    {ok, #{<<"members">> := MembersResponse4}} = client:naive_members(<<"zone1/Provider1">>),

    {ok, #{<<"reaches">> := ReachesResponse1}} = client:naive_reaches(<<"zone2/User3">>, <<"zone3/Provider3">>),
    {ok, #{<<"reaches">> := ReachesResponse2}} = client:naive_reaches(<<"zone3/User5">>, <<"zone3/Space4">>),
    {ok, #{<<"reaches">> := ReachesResponse3}} = client:naive_reaches(<<"zone2/User3">>, <<"zone2/Provider2">>),
    {ok, #{<<"reaches">> := ReachesResponse4}} = client:naive_reaches(<<"zone1/User1">>, <<"zone1/Provider1">>),

    {ok, #{<<"effectivePermissions">> := EPResponse1}} =
        client:naive_effective_permissions(<<"zone1/User1">>, <<"zone1/Provider1">>),
    {ok, #{<<"effectivePermissions">> := EPResponse2}} =
        client:naive_effective_permissions(<<"zone3/User5">>, <<"zone2/Provider2">>),
    {ok, #{<<"effectivePermissions">> := EPResponse3}} =
        client:naive_effective_permissions(<<"zone2/User3">>, <<"zone2/Provider2">>),
    
    ?assert(client_utils:lists_equal(MembersResponse1, [<<"zone1/User2">>,
        <<"zone2/User3">>, <<"zone2/Group3">>, <<"zone3/User4">>, <<"zone3/Group4">>])),
    ?assert(client_utils:lists_equal(MembersResponse2, [])),
    ?assert(client_utils:lists_equal(MembersResponse3, [])),
    ?assert(client_utils:lists_equal(MembersResponse4, [<<"zone1/User1">>, <<"zone1/Group1">>])),

    ?assertEqual(false, ReachesResponse1),
    ?assertEqual(false, ReachesResponse2),
    ?assertEqual(true, ReachesResponse3),
    ?assertEqual(true, ReachesResponse4),

    ?assertEqual(<<"01110">>, EPResponse1),
    ?assertEqual(<<"00000">>, EPResponse2),
    ?assertEqual(<<"00011">>, EPResponse3).
