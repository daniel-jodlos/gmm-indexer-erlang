-module(graph).
-author("Daniel Jodłoś").

%%%% @todo Add function that retrieves zone from the vertex's ID

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

-include("records.hrl").

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

%%% Parent, Child, Vertex to wszystko ID-ki, jesli wolisz mozesz zmienic nazwy na ParentId itd., jak uwazasz

-spec create_edge(Parent::binary(), Child::binary(), Permissions::binary()) -> ok | {error, string()}.
create_edge(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).

-spec update_edge(Parent::binary(), Child::binary(), Permissions::binary()) -> ok | {error, string()}.
update_edge(_Arg0, _Arg1, _Arg2) ->
    erlang:error(not_implemented).

-spec remove_edge(Parent::binary(), Child::binary()) -> ok | {error, string()}.
remove_edge(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

-spec edge_exists(Parent::binary(), Child::binary()) -> true | false | {error, string()}.
edge_exists(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

-spec get_edge(Parent::binary(), Child::binary()) -> {ok, map()} | {error, string()}.
get_edge(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

%% @todo pierwszy klucz w mapie to <<"parents">>, a drugi to <<"children">>, ale nie wiem jak to przekazac w -spec
-spec list_neighbours(Vertex::binary()) ->
    {ok, #{binary() := list(binary()), binary() := list(binary())}} | {error, string()}.
list_neighbours(_Arg0) ->
    erlang:error(not_implemented).

-spec list_parents(Vertex::binary()) -> {ok, list(binary())} | {error, string()}.
list_parents(_Arg0) ->
    erlang:error(not_implemented).

-spec list_children(Vertex::binary()) -> {ok, list(binary())} | {error, string()}.
list_children(_Arg0) ->
    erlang:error(not_implemented).

%% vertices api

generate_id() ->
    Id = << <<Y>> ||<<X:4>> <= crypto:hash(md5, term_to_binary(make_ref())), Y <- integer_to_list(X,16)>>,
    Zone = list_to_binary(?ZONE_ID),
    IdWithZone = << Zone/binary, "/", Id/binary >>,
    case persistence:if_exists(IdWithZone) of
        {ok, <<"0">>} -> {ok, Id};
        {ok, <<"1">>} -> generate_id();
        {error, Reason} -> {error, Reason}
    end.

create_vertex(Type, Name) ->
    {ok, Id} = generate_id(),
    Json = json_utils:encode(#{
        <<"type">> => Type,
        <<"id">> => Id,
        <<"name">> => Name,
        <<"zone">> => list_to_binary(?ZONE_ID)
    }),
    case persistence:set(Id, Json) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> {ok, Id}
    end.

update_vertex(Id, NewName) ->
    {ok, Vertex} = get_vertex(Id),
    Data = json_utils:decode(Vertex),
    Json = json_utils:encode(maps:update(<<"name">>, NewName, Data)),
    case persistence:set(Id, Json) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

remove_vertex(Id) ->
    case persistence:del(Id) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

vertex_exists(Key) ->
    case persistence:if_exists(Key) of
        {ok, <<"0">>} -> false;
        {ok, <<"1">>} -> true;
        {error, Reason} -> {error, Reason}
    end.

get_vertex(Id) ->
    case vertex_exists(Id) of
        false -> {error, nonexisting_vertex};
        true -> persistence:get(Id)
    end.

vertices_to_types(IdsMap, []) ->
    {ok, IdsMap};
vertices_to_types(IdsMap, [Key | Rest]) ->
    {ok, Vertex} = get_vertex(Key),
    case get_vertex(Key) of
        {ok, Vertex} ->
            Data = json_utils:decode(Vertex),
            case maps:get(<<"type">>, Data) of
                #vertex_type.user ->
                    Users = maps:get(<<"users">>, IdsMap),
                    vertices_to_types(maps:update(<<"users">>, Users ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                #vertex_type.group ->
                    Groups = maps:get(<<"groups">>, IdsMap),
                    vertices_to_types(maps:update(<<"groups">>, Groups ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                #vertex_type.space ->
                    Spaces = maps:get(<<"spaces">>, IdsMap),
                    vertices_to_types(maps:update(<<"spaces">>, Spaces ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                #vertex_type.provider ->
                    Providers = maps:get(<<"providers">>, IdsMap),
                    vertices_to_types(maps:update(<<"providers">>, Providers ++ [maps:get(<<"id">>, Data)], IdsMap), Rest)
            end;
        {error, Reason} -> {error, Reason}
    end.

list_vertices() ->
    {ok, Keys} = persistence:keys("*"),
    vertices_to_types(#{
        <<"users">> => [],
        <<"groups">> => [],
        <<"spaces">> => [],
        <<"providers">> => []
    }, Keys).

get_vertices_of_type(_Type, IdsList, []) ->
    {ok, IdsList};
get_vertices_of_type(Type, IdsList, [Key | Rest]) ->
    case get_vertex(Key) of
        {ok, Vertex} ->
            Data = json_utils:decode(Vertex),
            case maps:get(<<"type">>, Data) of
                Type -> get_vertices_of_type(Type, IdsList ++ [maps:get(<<"id">>, Data)], Rest);
                _ -> get_vertices_of_type(Type, IdsList, Rest)
            end;
        {error, Reason} -> {error, Reason}
    end.

list_vertices(Type) ->
    Keys = persistence:keys("*"),
    get_vertices_of_type(Type, [], Keys).

%% edges api - todo
