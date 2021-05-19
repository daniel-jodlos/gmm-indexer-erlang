-module(graph).
-author("Daniel Jodłoś").

%%%% @todo Old API, you can remove it

%%-export([
%%    vertex_exists/1,
%%    get_vertex/1,
%%    get_vertices/0,
%%    add_user/1,
%%    delete/1,
%%    update_user/2,
%%    add_group/1,
%%    add_user_to_group/2,
%%    is_member_of_group/2,
%%    list_group_users/1,
%%    remove_user_from_group/2,
%%    create_edge/3
%%]).

%% API for vertices
-export([
    create_vertex/2,
    update_vertex/2,
    remove_vertex/1,
    vertex_exists/1,
    get_vertex/1,
    list_vertices/0,
    list_vertices/1
]).

%% API for edges
-export([
    create_edge/3,
    update_edge/3,
    remove_edge/2,
    edge_exists/2,
    get_edge/2,
    list_neighbours/1,
    list_parents/1,
    list_children/1
]).

%%%% @todo You can use/rename any of those, your business

%%generate_id() ->
%%    Id = << <<Y>> ||<<X:4>> <= crypto:hash(md5, term_to_binary(make_ref())), Y <- integer_to_list(X,16)>>,
%%    case persistence:if_exists(Id) of
%%        {ok, 0} -> Id;
%%        {ok, 1} -> generate_id();
%%        {error, Reason} -> {error, Reason}
%%    end.
%%
%%get_vertex(Id) ->
%%    persistence:get(Id).
%%
%%get_vertices()->
%%    persistence:keys("*").
%%
%%add_user(Name) ->
%%    Id = generate_id(),
%%    Json = json_utils:encode(#{<<"type">> => <<"user">>, <<"id">> => Id, <<"name">> => Name}),
%%    Response = persistence:set(Id, Json),
%%    case Response of
%%        {error, Reason} -> {error, Reason};
%%        {ok, _Result} -> {ok, Id}
%%    end.
%%
%%update_user(Id, NewName) ->
%%    Json = json_utils:encode(#{<<"type">> => <<"user">>, <<"id">> => Id, <<"Name">> => NewName}),
%%    Response = persistence:set(Id, Json),
%%    case Response of
%%        {error, Reason} -> {error, Reason};
%%        {ok, _Result} -> ok
%%    end.
%%
%%delete(Id) ->
%%    Response = persistence:del(Id),
%%    case Response of
%%        {error, Reason} -> {error, Reason};
%%        {ok, _Result} -> ok
%%    end.
%%
%%children_key(Key) when is_binary(Key) == true ->
%%    <<Key/binary, "/children">>;
%%children_key(Key) -> children_key(binary:list_to_bin(Key)).
%%
%%add_user_to_group(User, Group) ->
%%    persistence:set_add(children_key(Group), User).
%%
%%remove_user_from_group(User, Group) ->
%%    persistence:set_remove(children_key(Group), User).
%%
%%is_member_of_group(User, Group) ->
%%    {ok, Result} = persistence:set_is_member(children_key(Group), User),
%%    Result.
%%
%%list_group_users(Group) ->
%%    {ok, Result} = persistence:set_list_members(children_key(Group)),
%%    Result.
%%
%%
%%add_group(Name) ->
%%    Id = generate_id(),
%%    Json = json_utils:encode(#{<<"type">> => <<"group">>, <<"id">> => Id, <<"name">> => Name}),
%%    Response = persistence:set(Id, Json),
%%    case Response of
%%        {error, Reason} -> {error, Reason};
%%        {ok, _Result} -> {ok, Id}
%%    end.
%%
%%
%%
%%create_edge(Parent, Child, Permissions) ->
%%    Flag = case {vertex_exists(Parent), vertex_exists(Child)} of
%%               {false, _} -> false;
%%               {_, false} -> false;
%%               _ -> true
%%           end,
%%    case Flag of
%%        false -> {error, "At least one of nodes doesn't exist"};
%%        true ->
%%            persistence:add_parent(Child, Parent, Permissions),
%%            persistence:add_child(Parent, Child, Permissions),
%%            ok
%%    end.
%%
%%vertex_exists(Key) ->
%%    case persistence:if_exists(Key) of
%%        {ok, 0} -> false;
%%        {ok, 1} -> true;
%%        _ -> error
%%    end.

%%%% @todo Here we go

create_vertex(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

update_vertex(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

remove_vertex(_Arg0) ->
    erlang:error(not_implemented).

vertex_exists(_Arg0) ->
    erlang:error(not_implemented).

get_vertex(_Arg0) ->
    erlang:error(not_implemented).

list_vertices() ->
    erlang:error(not_implemented).

list_vertices(_Arg0) ->
    erlang:error(not_implemented).

create_edge(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).

update_edge(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).

remove_edge(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

edge_exists(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

get_edge(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

list_neighbours(_Arg0) ->
    erlang:error(not_implemented).

list_parents(_Arg0) ->
    erlang:error(not_implemented).

list_children(_Arg0) ->
    erlang:error(not_implemented).