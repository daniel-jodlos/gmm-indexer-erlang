-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
    all/0,
    init_per_suite/1
]).
-export([users_adding_test/1]).

all() -> [users_adding_test/1].

init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

%% TODO: needs testing
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
    client:add_group("Group4"),
    {ok, Listing} = client:get_vertices_list(),
    2 = length(maps:get("users", Listing)),
    3 = length(maps:get("providers", Listing)),
    1 = length(maps:get("spaces", Listing)),
    4 = length(maps:get("groups", Listing)),
    ok.