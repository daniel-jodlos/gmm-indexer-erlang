-module(naive_queries_redis_outside_SUITE).
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
    ok = client:add_vertex(<<"zone0">>, <<"user">>, <<"User1">>),
    ok = client:add_vertex(<<"zone0">>, <<"user">>, <<"User2">>),
    ok = client:add_vertex(<<"zone1">>, <<"user">>, <<"User3">>),
    ok = client:add_vertex(<<"zone2">>, <<"user">>, <<"User4">>),
    ok = client:add_vertex(<<"zone2">>, <<"user">>, <<"User5">>),
    ok = client:add_vertex(<<"zone0">>, <<"group">>, <<"Group1">>),
    ok = client:add_vertex(<<"zone1">>, <<"group">>, <<"Group2">>),
    ok = client:add_vertex(<<"zone1">>, <<"group">>, <<"Group3">>),
    ok = client:add_vertex(<<"zone2">>, <<"group">>, <<"Group4">>),
    ok = client:add_vertex(<<"zone0">>, <<"space">>, <<"Space1">>),
    ok = client:add_vertex(<<"zone0">>, <<"space">>, <<"Space2">>),
    ok = client:add_vertex(<<"zone1">>, <<"space">>, <<"Space3">>),
    ok = client:add_vertex(<<"zone2">>, <<"space">>, <<"Space4">>),
    ok = client:add_vertex(<<"zone0">>, <<"provider">>, <<"Provider1">>),
    ok = client:add_vertex(<<"zone1">>, <<"provider">>, <<"Provider2">>),
    ok = client:add_vertex(<<"zone2">>, <<"provider">>, <<"Provider3">>),

    ok = client:add_edge(<<"zone0:User1">>, <<"zone0:Group1">>, <<"01110">>, <<"trace1">>),
    ok = client:add_edge(<<"zone0:Group1">>, <<"zone0:Provider1">>, <<"10101">>, <<"trace2">>),
    ok = client:add_edge(<<"zone0:User2">>, <<"zone2:Space4">>, <<"00100">>, <<"trace3">>),
    ok = client:add_edge(<<"zone1:User3">>, <<"zone1:Group3">>, <<"00011">>, <<"trace4">>),
    ok = client:add_edge(<<"zone1:Group3">>, <<"zone1:Group2">>, <<"00010">>, <<"trace5">>),
    ok = client:add_edge(<<"zone1:Group2">>, <<"zone0:Space2">>, <<"10000">>, <<"trace6">>),
    ok = client:add_edge(<<"zone1:Space3">>, <<"zone1:Provider2">>, <<"01100">>, <<"trace7">>),
    ok = client:add_edge(<<"zone2:User4">>, <<"zone1:Group3">>, <<"01000">>, <<"trace8">>),
    ok = client:add_edge(<<"zone1:Group3">>, <<"zone2:Group4">>, <<"10000">>, <<"trace9">>),
    ok = client:add_edge(<<"zone2:User5">>, <<"zone2:Group4">>, <<"01101">>, <<"trace10">>),
    ok = client:add_edge(<<"zone2:Group4">>, <<"zone2:Space4">>, <<"10111">>, <<"trace11">>),
    ok = client:add_edge(<<"zone2:Space4">>, <<"zone1:Provider2">>, <<"11110">>, <<"trace12">>),

    % then
    {ok, #{<<"members">> := MembersResponse1}} = client:naive_members(<<"zone2:Space4">>),
    {ok, #{<<"members">> := MembersResponse2}} = client:naive_members(<<"zone1:Group2">>),
    {ok, #{<<"members">> := MembersResponse3}} = client:naive_members(<<"zone0:Space1">>),
    {ok, #{<<"members">> := MembersResponse4}} = client:naive_members(<<"zone0:Provider1">>),

    {ok, #{<<"reaches">> := ReachesResponse1}} = client:naive_reaches(<<"zone1:User3">>, <<"zone2:Provider3">>),
    {ok, #{<<"reaches">> := ReachesResponse2}} = client:naive_reaches(<<"zone2:User5">>, <<"zone2:Space4">>),
    {ok, #{<<"reaches">> := ReachesResponse3}} = client:naive_reaches(<<"zone1:User3">>, <<"zone1:Provider2">>),
    {ok, #{<<"reaches">> := ReachesResponse4}} = client:naive_reaches(<<"zone0:User1">>, <<"zone0:Provider1">>),

    {ok, #{<<"effectivePermissions">> := EPResponse1}} =
        client:naive_effective_permissions(<<"zone0:User1">>, <<"zone0:Provider1">>),
    {ok, #{<<"effectivePermissions">> := EPResponse2}} =
        client:naive_effective_permissions(<<"zone2:User5">>, <<"zone1:Provider2">>),
    {ok, #{<<"effectivePermissions">> := EPResponse3}} =
        client:naive_effective_permissions(<<"zone1:User3">>, <<"zone1:Provider2">>),

    ?assert(client_utils:lists_equal(MembersResponse1, [<<"zone0:User2">>,
        <<"zone1:User3">>, <<"zone1:Group3">>, <<"zone2:User4">>, <<"zone2:User5">>, <<"zone2:Group4">>])),
    ?assert(client_utils:lists_equal(MembersResponse2, [<<"zone1:User3">>, <<"zone1:Group3">>, <<"zone2:User4">>])),
    ?assert(client_utils:lists_equal(MembersResponse3, [])),
    ?assert(client_utils:lists_equal(MembersResponse4, [<<"zone0:User1">>, <<"zone0:Group1">>])),

    ?assertEqual(false, ReachesResponse1),
    ?assertEqual(true, ReachesResponse2),
    ?assertEqual(true, ReachesResponse3),
    ?assertEqual(true, ReachesResponse4),

    ?assertEqual(<<"10101">>, EPResponse1),
    ?assertEqual(<<"11110">>, EPResponse2),
    ?assertEqual(<<"11110">>, EPResponse3).

operations_after_graph_update_test(_Config) ->
    % when
    ok = client:delete_edge(<<"zone1:Group3">>, <<"zone1:Group2">>, <<"trace5">>),
    ok = client:delete_edge(<<"zone2:User5">>, <<"zone2:Group4">>, <<"trace10">>),
    ok = client:set_permissions(<<"zone1:Group3">>, <<"zone2:Group4">>, <<"00010">>, <<"trace9">>),
    ok = client:set_permissions(<<"zone2:Space4">>, <<"zone1:Provider2">>, <<"01010">>, <<"trace12">>),
    ok = client:add_edge(<<"zone1:Group2">>, <<"zone2:Provider3">>, <<"01101">>, <<"trace13">>),
    {error, _} = client:add_edge(<<"zone2:User5">>, <<"zone2:Space3">>, <<"01101">>, <<"trace14">>),
    %% @todo I think that there should be separate test for the case when we want edge creation to fail
    %%       and I suspect that it wasn't intentional, which suggests to check all those tests once more

    % then
    {ok, #{<<"members">> := MembersResponse1}} = client:naive_members(<<"zone2:Space4">>),
    {ok, #{<<"members">> := MembersResponse2}} = client:naive_members(<<"zone1:Group2">>),
    {ok, #{<<"members">> := MembersResponse3}} = client:naive_members(<<"zone0:Space1">>),
    {ok, #{<<"members">> := MembersResponse4}} = client:naive_members(<<"zone0:Provider1">>),

    {ok, #{<<"reaches">> := ReachesResponse1}} = client:naive_reaches(<<"zone1:User3">>, <<"zone2:Provider3">>),
    {ok, #{<<"reaches">> := ReachesResponse2}} = client:naive_reaches(<<"zone2:User5">>, <<"zone2:Space4">>),
    {ok, #{<<"reaches">> := ReachesResponse3}} = client:naive_reaches(<<"zone1:User3">>, <<"zone1:Provider2">>),
    {ok, #{<<"reaches">> := ReachesResponse4}} = client:naive_reaches(<<"zone0:User1">>, <<"zone0:Provider1">>),

    {ok, #{<<"effectivePermissions">> := EPResponse1}} =
        client:naive_effective_permissions(<<"zone0:User1">>, <<"zone0:Provider1">>),
    {ok, #{<<"effectivePermissions">> := EPResponse2}} =
        client:naive_effective_permissions(<<"zone2:User5">>, <<"zone1:Provider2">>),
    {ok, #{<<"effectivePermissions">> := EPResponse3}} =
        client:naive_effective_permissions(<<"zone1:User3">>, <<"zone1:Provider2">>),
    
    ?assert(client_utils:lists_equal(MembersResponse1, [<<"zone0:User2">>,
        <<"zone1:User3">>, <<"zone1:Group3">>, <<"zone2:User4">>, <<"zone2:Group4">>])),
    ?assert(client_utils:lists_equal(MembersResponse2, [])),
    ?assert(client_utils:lists_equal(MembersResponse3, [])),
    ?assert(client_utils:lists_equal(MembersResponse4, [<<"zone0:User1">>, <<"zone0:Group1">>])),

    ?assertEqual(false, ReachesResponse1),
    ?assertEqual(false, ReachesResponse2),
    ?assertEqual(true, ReachesResponse3),
    ?assertEqual(true, ReachesResponse4),

    ?assertEqual(<<"10101">>, EPResponse1),
    ?assertEqual(<<"00000">>, EPResponse2),
    ?assertEqual(<<"01010">>, EPResponse3).
