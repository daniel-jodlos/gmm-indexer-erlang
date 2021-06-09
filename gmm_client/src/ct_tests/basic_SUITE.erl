-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
    all/0,
    groups/0,
    init_per_suite/1
]).
-export([
    users_adding_test/1
    % users_listing_getting_test/1,
    % users_deleting_test/1,
    % users_listing_getting_test_2/1
]).

groups() -> [{basic_flow, [sequence], [
    users_adding_test
    % users_listing_getting_test,
    % users_deleting_test,
    % users_listing_getting_test_2
]}].

all() -> [{group, basic_flow}].

init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

users_adding_test(_Config) ->
    client:add_user("User1"),
    client:add_user("User2"),
    client:add_provider("Provider1"),
    client:add_provider("Provider2"),
    client:add_provider("Provider3"),
    client:add_space("Space"),
    client:add_group("Group1"),
    client:add_group("Group2"),
    client:add_group("Group3"),
    client:add_group("Group4").

% users_listing_getting_test(_Config) ->
%     {ok, Listing} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
%     2 = length(maps:get("users", Listing)),
%     3 = length(maps:get("providers", Listing)),
%     1 = length(maps:get("spaces", Listing)),
%     4 = length(maps:get("groups", Listing)).

% users_deleting_test(_Config) ->
%     client:delete_vertex("zone1/User2"),
%     client:delete_vertex("zone1/Provider1"),
%     client:delete_vertex("zone1/Provider2"),
%     client:delete_vertex("zone1/Provider3"),
%     client:delete_vertex("zone1/Group2"),
%     client:delete_vertex("zone1/Group3").

% users_listing_getting_test_2(_Config) ->
%     {ok, Listing} = client_requests:get_simple_request_body(?URL++"graph/vertices/listing"),
%     1 = length(maps:get("users", Listing)),
%     0 = length(maps:get("providers", Listing)),
%     1 = length(maps:get("spaces", Listing)),
%     2 = length(maps:get("groups", Listing)).