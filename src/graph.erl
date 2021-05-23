-module(graph).
-author("Daniel Jodłoś").

%%%% @todo Add function that retrieves zone from the vertex's ID

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


%% vertices api

generate_id() ->
    Id = << <<Y>> ||<<X:4>> <= crypto:hash(md5, term_to_binary(make_ref())), Y <- integer_to_list(X,16)>>,
    Zone = list_to_binary(?ZONE_ID),
    IdWithZone = << Zone/binary, "_", Id/binary >>,
    case persistence:exists(IdWithZone) of
        {ok, <<"0">>} -> {ok, IdWithZone};
        {ok, <<"1">>} -> generate_id();
        {error, Reason} -> {error, Reason}
    end.

-spec create_vertex(Type::binary(), Name::binary()) -> {ok, binary()} | {error, any()}.
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

-spec update_vertex(Id::binary(), NewName::binary()) -> ok | {error, any()}.
update_vertex(Id, NewName) ->
    {ok, Vertex} = get_vertex(Id),
    Data = json_utils:decode(Vertex),
    Json = json_utils:encode(maps:update(<<"name">>, NewName, Data)),
    case persistence:set(Id, Json) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

-spec remove_vertex(Id::binary()) -> ok | {error, any()}.
remove_vertex(Id) ->
    case persistence:del(Id) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

-spec vertex_exists(Key::binary()) -> {ok, boolean()} | {error, any()}.
vertex_exists(Key) ->
    case persistence:exists(Key) of
        {ok, <<"0">>} -> {ok, false};
        {ok, <<"1">>} -> {ok, true};
        {error, Reason} -> {error, Reason}
    end.

-spec get_vertex(Id::binary()) -> {ok, map()} | {error, any()}.
get_vertex(Id) ->
    case vertex_exists(Id) of
        {ok, true} -> persistence:get(Id);
        {ok, false} -> {error, vertex_not_existing};
        {error, Reason} -> {error, Reason}
    end.

vertices_to_types(IdsMap, []) ->
    {ok, IdsMap};
vertices_to_types(IdsMap, [Key | Rest]) ->
    {ok, Vertex} = get_vertex(Key),
    case get_vertex(Key) of
        {ok, Vertex} ->
            Data = json_utils:decode(Vertex),
            case maps:get(<<"type">>, Data) of
                <<"user">> ->
                    Users = maps:get(<<"users">>, IdsMap),
                    vertices_to_types(maps:update(<<"users">>, Users ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                <<"group">> ->
                    Groups = maps:get(<<"groups">>, IdsMap),
                    vertices_to_types(maps:update(<<"groups">>, Groups ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                <<"space">> ->
                    Spaces = maps:get(<<"spaces">>, IdsMap),
                    vertices_to_types(maps:update(<<"spaces">>, Spaces ++ [maps:get(<<"id">>, Data)], IdsMap), Rest);
                <<"provider">> ->
                    Providers = maps:get(<<"providers">>, IdsMap),
                    vertices_to_types(maps:update(<<"providers">>, Providers ++ [maps:get(<<"id">>, Data)], IdsMap), Rest)
            end;
        {error, Reason} -> {error, Reason}
    end.

-spec list_vertices() -> {ok, map()} | {error, any()}.
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

-spec list_vertices(Type::binary()) -> {ok, list()} | {error, any()}.
list_vertices(Type) ->
    Keys = persistence:keys("*"),
    get_vertices_of_type(Type, [], Keys).


%% edges api - todo

childrens_id(Parent) -> <<Parent/binary, "/children">>.
parents_id(Child) -> <<Child/binary, "/parents">>.
edge_id(Parent, Child) -> <<"edge/", Parent/binary, "/", Child/binary>>.

-spec validate([any() | {error, any()}]) -> ok | {error, any()}.
validate(Results) ->
    Reducer = fun
        ({error, NError}, {error, Error}) -> {error, [NError | Error]};
        ({error, Error}, _) -> {error, Error};
        (_, {error, Error}) -> {error, Error};
        (_, _) -> ok
    end,
    lists:foldl(Reducer, ok, Results).

%%%% Parent, Child, Vertex to wszystko ID-ki, jesli wolisz mozesz zmienic nazwy na ParentId itd., jak uwazasz

-spec create_edge(Parent::binary(), Child::binary(), Permissions::binary()) -> ok | {error, any()}.
create_edge(Parent, Child, Permissions) -> validate([
        persistence:set_add(childrens_id(Parent), Child),
        persistence:set_add(parents_id(Child), Parent),
        persistence:set(edge_id(Parent, Child), Permissions)
    ]).

-spec update_edge(Parent::binary(), Child::binary(), Permissions::binary()) -> ok | {error, any()}.
update_edge(Parent, Child, Permissions) -> validate([
        persistence:set(edge_id(Parent, Child), Permissions)
    ]).

-spec remove_edge(Parent::binary(), Child::binary()) -> ok | {error, any()}.
remove_edge(Parent, Child) -> validate([
        persistence:del(edge_id(Parent, Child)),
        persistence:set_remove(childrens_id(Parent), Child),
        persistence:set_remove(parents_id(Child), Parent)
    ]).

-spec edge_exists(Parent::binary(), Child::binary()) -> {ok, boolean()} | {error, any()}.
edge_exists(Parent, Child) ->
    persistence:exists(edge_id(Parent, Child)).

-spec get_edge(Parent::binary(), Child::binary()) -> {ok, map()} | {error, any()}.
get_edge(Parent, Child) ->
    case persistence:get(edge_id(Parent, Child)) of
        {error, Error} -> {error, Error};
        {ok, Permissions} -> {ok, #{ <<"Parent">> => Parent, <<"Child">> => Child, <<"Permissions">> => Permissions }}
    end.

%% #{<<"parents">> => list(binary()), <<"children">> => list(binary())}
-spec list_neighbours(Vertex::binary()) ->
    {ok, #{binary() := list(binary()), binary() := list(binary())}} | {error, any()}.
list_neighbours(Vertex) ->
    Parents = list_parents(Vertex),
    Children = list_children(Vertex),
    case lists:any(fun ({error, _}) -> true; (_) -> false end, [Parents, Children]) of
        true -> {error, "Vertex doesn't exist"};
        false -> #{ <<"Parents">> => Parents, <<"Children">> => Children }
    end.

-spec list_parents(Vertex::binary()) -> {ok, list(binary())} | {error, any()}.
list_parents(Vertex) ->
    persistence:set_list_members(parents_id(Vertex)).


-spec list_children(Vertex::binary()) -> {ok, list(binary())} | {error, any()}.
list_children(Vertex) ->
    persistence:set_list_members(childrens_id(Vertex)).
