%%%-------------------------------------------------------------------
%% @doc
%%  Declares API and implements functions to operate on the graph
%% @end
%%%-------------------------------------------------------------------

-module(graph).

-author("Daniel Jodłoś").

% Naming convention for egdes
% - From/To, or Parent/Child, is Id of a neighbour of current vertex
%
% Edge is directed as follows:  {Parent -> Child} or {From -> To}
% For example from group to user

%% @todo Ujednolicic nazewnictwo pol w zwracanych mapach
%% @todo - albo wszystkie od wielkiej litery, albo wszystkie od malej

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

%%%---------------------------
%% VERTICES API
%%%---------------------------

-spec generate_id(binary()) -> {ok, binary()} | {error, any()}.
generate_id(Name) ->
    Zone = list_to_binary(?ZONE_ID),
    Id = gmm_utils:create_vertex_id(Name, Zone),
%%    IdWithZone = <<Zone/binary, "_", Id/binary>>,
    case persistence:exists(Id) of
        {ok, false} -> {ok, Id};
        {ok, true} -> {error, "Vertex exists"};
        {error, Reason} -> {error, Reason}
    end.


-spec create_vertex(Type :: binary(), Name :: binary()) -> {ok, binary()} | {error, any()}.
create_vertex(Type, Name) ->
    {ok, Id} = generate_id(Name),
    Json =
        gmm_utils:encode(#{
            <<"type">> => Type,
            <<"id">> => Id,
            <<"name">> => Name,
            <<"zone">> => list_to_binary(?ZONE_ID)
        }),
    case persistence:set(Id, Json) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> {ok, Id}
    end.


-spec update_vertex(Id :: binary(), NewName :: binary()) -> ok | {error, any()}.
update_vertex(Id, NewName) ->
    {ok, Data} = get_vertex(Id),
    Json = gmm_utils:encode(maps:update(<<"name">>, NewName, Data)),
    case persistence:set(Id, Json) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.


-spec remove_vertex(Id :: binary()) -> ok | {error, any()}.
remove_vertex(Id) ->
    case persistence:del(Id) of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.


-spec vertex_exists(Key :: binary()) -> {ok, boolean()} | {error, any()}.
vertex_exists(Key) -> persistence:exists(Key).

-spec get_vertex(Id :: binary()) -> {ok, map()} | {error, any()}.
get_vertex(Id) ->
    case vertex_exists(Id) of
        {ok, true} ->
            case persistence:get(Id) of
                {ok, Data} -> {ok, gmm_utils:decode(Data)};
                {error, Reason} -> {error, Reason}
            end;

        {ok, false} -> {error, vertex_not_existing};
        {error, Reason} -> {error, Reason}
    end.


is_children_list(Id) ->
    case re:run(Id, <<"/children">>) of
        nomatch -> false;
        _ -> true
    end.


is_parents_list(Id) ->
    case re:run(Id, <<"/parents">>) of
        nomatch -> false;
        _ -> true
    end.


is_edge(Id) ->
    case re:run(Id, <<"edge/">>) of
        nomatch -> false;
        _ -> true
    end.


is_vertex(Id) ->
    (is_children_list(Id) =:= false) and (is_parents_list(Id) =:= false) and (is_edge(Id) =:= false).


vertices_to_types(IdsMap, []) ->
    {ok, IdsMap};
vertices_to_types(IdsMap, [Key | Rest]) ->
    case get_vertex(Key) of
        {ok, Data} ->
            case maps:get(<<"type">>, Data) of
                <<"user">> ->
                    Users = maps:get(<<"users">>, IdsMap),
                    vertices_to_types(
                        maps:update(<<"users">>, Users ++ [maps:get(<<"id">>, Data)], IdsMap),
                        Rest
                    );
                <<"group">> ->
                    Groups = maps:get(<<"groups">>, IdsMap),
                    vertices_to_types(
                        maps:update(<<"groups">>, Groups ++ [maps:get(<<"id">>, Data)], IdsMap),
                        Rest
                    );
                <<"space">> ->
                    Spaces = maps:get(<<"spaces">>, IdsMap),
                    vertices_to_types(
                        maps:update(<<"spaces">>, Spaces ++ [maps:get(<<"id">>, Data)], IdsMap),
                        Rest
                    );
                <<"provider">> ->
                    Providers = maps:get(<<"providers">>, IdsMap),
                    vertices_to_types(
                        maps:update(<<"providers">>, Providers ++ [maps:get(<<"id">>, Data)], IdsMap),
                        Rest
                    )
            end;

        {error, Reason} -> {error, Reason}
    end.


-spec list_vertices() -> {ok, map()} | {error, any()}.
list_vertices() ->
    {ok, Keys} = persistence:keys("*"),
    FilteredKeys = lists:filter(fun (X) -> is_vertex(X) =:= true end, Keys),
    vertices_to_types(
        #{<<"users">> => [], <<"groups">> => [], <<"spaces">> => [], <<"providers">> => []},
        FilteredKeys
    ).


get_vertices_of_type(_Type, IdsList, []) ->
    {ok, IdsList};
get_vertices_of_type(Type, IdsList, [Key | Rest]) ->
    case get_vertex(Key) of
        {ok, Data} ->
            case maps:get(<<"type">>, Data) of
                Type -> get_vertices_of_type(Type, IdsList ++ [maps:get(<<"id">>, Data)], Rest);
                _ -> get_vertices_of_type(Type, IdsList, Rest)
            end;
        {error, Reason} -> {error, Reason}
    end.


-spec list_vertices(Type :: binary()) -> {ok, list()} | {error, any()}.
list_vertices(Type) ->
    Keys = persistence:keys("*"),
    FilteredKeys = lists:filter(fun (X) -> is_vertex(X) =:= true end, Keys),
    get_vertices_of_type(Type, [], FilteredKeys).

%%%---------------------------
%% EDGES API
%%%---------------------------

children_id(From) -> <<From/binary, "/children">>.

parents_id(To) -> <<To/binary, "/parents">>.

edge_id(From, To) -> <<"edge/", From/binary, "/", To/binary>>.

-spec validate([any() | {error, any()}]) -> ok | {error, any()}.
validate(Results) ->
    Reducer =
        fun
            ({error, NError}, {error, Error}) -> {error, [NError | Error]};
            ({error, Error}, _) -> {error, Error};
            (_, {error, Error}) -> {error, Error};
            (_, _) -> ok
        end,
    lists:foldl(Reducer, ok, Results).


-spec create_edge(From :: binary(), To :: binary(), Permissions :: binary()) -> ok | {error, any()}.
create_edge(From, To, Permissions) ->
    validate([
        persistence:set_add(children_id(From), To),
        persistence:set_add(parents_id(To), From),
        persistence:set(edge_id(From, To), Permissions)
    ]).

-spec update_edge(From :: binary(), To :: binary(), Permissions :: binary()) -> ok | {error, any()}.
update_edge(From, To, Permissions) -> validate([persistence:set(edge_id(From, To), Permissions)]).

-spec remove_edge(From :: binary(), To :: binary()) -> ok | {error, any()}.
remove_edge(From, To) ->
    validate([
        persistence:del(edge_id(From, To)),
        persistence:set_remove(children_id(From), To),
        persistence:set_remove(parents_id(To), From)
    ]).

-spec edge_exists(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
edge_exists(From, To) -> persistence:exists(edge_id(From, To)).

-spec get_edge(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
get_edge(From, To) ->
    case persistence:get(edge_id(From, To)) of
        {error, Error} -> {error, Error};

        {ok, Permissions} ->
            {ok, #{<<"From">> => From, <<"To">> => To, <<"Permissions">> => Permissions}}
    end.

%% #{<<"parents">> => list(binary()), <<"children">> => list(binary())}
-spec list_neighbours(Vertex :: binary()) ->
  {ok, #{binary() := list(binary()), binary() := list(binary())}} | {error, any()}.
list_neighbours(Vertex) ->
    Parents = list_parents(Vertex),
    Children = list_children(Vertex),
    case lists:any(fun ({error, _}) -> true; (_) -> false end, [Parents, Children]) of
        true -> {error, "Vertex doesn't exist"};
        false -> #{<<"parents">> => Parents, <<"children">> => Children}
    end.


-spec list_parents(Vertex :: binary()) -> {ok, list(binary())} | {error, any()}.
list_parents(Vertex) -> persistence:set_list_members(parents_id(Vertex)).

-spec list_children(Vertex :: binary()) -> {ok, list(binary())} | {error, any()}.
list_children(Vertex) -> persistence:set_list_members(children_id(Vertex)).
