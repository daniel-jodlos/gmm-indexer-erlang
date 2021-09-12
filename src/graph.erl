%%%-------------------------------------------------------------------
%% @doc
%%  Declares API and implements functions to operate on the graph
%% @end
%%%-------------------------------------------------------------------

-module(graph).

-author("Daniel Jodłoś").

% Naming convention for egdes
% - From==Child, or To==Parent, is Id of a neighbour of current vertex
%
% Edge is directed as follows:  {Child -> Parent} or {From -> To}
% For example from user to group

%% API for vertices
-export([
    create_vertex/2,
    update_vertex/2,
    remove_vertex/1,
    vertex_exists/1,
    get_vertex/1,
    list_vertices/0,
    list_vertices/1,
    test/0
]).

%% API for edges
-export([
    create_edge/3,
    create_effective_edge/3,
    update_edge/3,
    update_effective_edge/3,
    remove_edge/2,
    remove_effective_edge/2,
    edge_exists/2,
    effective_edge_exists/2,
    get_edge/2,
    get_effective_edge/2,
    list_neighbours/1,
    list_parents/1,
    list_children/1,
    effective_list_parents/1,
    effective_list_children/1
]).

%% additional functions
-export([
    all_zones/0
]).

-include("records.hrl").

%%%---------------------------
%% VERTICES API
%%%---------------------------

-spec generate_id(binary()) -> {ok, binary()} | {error, any()}.
generate_id(Name) ->
    Id = gmm_utils:create_vertex_id(Name),
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
            <<"zone">> => gmm_utils:zone_id()
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

effective_children_id(From) -> <<From/binary, "/effective_children">>.
effective_parents_id(To) -> <<To/binary, "/effective_parents">>.
effective_edge_id(From, To) ->
  Edge = edge_id(From, To),
  <<"effective", Edge/binary>>.

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


-spec create_edge(From :: binary(), To :: binary(), Permissions :: permissions()) -> ok | {error, any()}.
create_edge(From, To, Permissions) ->
    ZoneId = gmm_utils:zone_id(),
    FromZone = gmm_utils:owner_of(From),
    ToZone = gmm_utils:owner_of(To),
    io:format("Creating edge from ~p to ~p with permissions of ~p~n", [From, To, Permissions]),
    post(From, {parent, updated, From, To, Permissions}),
    post(To, {child, updated, To, From, Permissions}),
    post_events_about_effective_children(From, To, updated, Permissions),
    post_events_about_effective_parents(To, From, updated, Permissions),
    case {FromZone, ToZone} of
        {ZoneId, ZoneId} -> validate([
            persistence:set_add(children_id(To), From),
            persistence:set_add(parents_id(From), To),
            persistence:set(edge_id(From, To), Permissions)
        ]);
        {ZoneId, _} -> validate([
            persistence:set_add(parents_id(From), To),
            persistence:set(edge_id(From, To), Permissions)
        ]);
        {_, ZoneId} -> validate([
            persistence:set_add(children_id(To), From),
            persistence:set(edge_id(From, To), Permissions)
        ]);
        {_, _} -> {error, vertices_not_found}
    end.

-spec create_effective_edge(From :: binary(), To :: binary(), Permissions :: permissions()) -> ok | {error, any()}.
create_effective_edge(From, To, Permissions) ->
    ZoneId = gmm_utils:zone_id(),
    FromZone = gmm_utils:owner_of(From),
    ToZone = gmm_utils:owner_of(To),
    case {FromZone, ToZone} of
        {ZoneId, ZoneId} -> validate([
            persistence:set_add(effective_children_id(To), From),
            persistence:set_add(effective_parents_id(From), To),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {ZoneId, _} -> validate([
            persistence:set_add(effective_parents_id(From), To),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {_, ZoneId} -> validate([
            persistence:set_add(effective_children_id(To), From),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {_, _} -> {error, vertices_not_found}
    end.

-spec create_effective_edge(From :: binary(), To :: binary(), Permissions :: permissions()) -> ok | {error, any()}.
create_effective_edge(From, To, Permissions) ->
    ZoneId = gmm_utils:zone_id(),
    FromZone = gmm_utils:owner_of(From),
    ToZone = gmm_utils:owner_of(To),
    case {FromZone, ToZone} of
        {ZoneId, ZoneId} -> validate([
            persistence:set_add(effective_children_id(To), From),
            persistence:set_add(effective_parents_id(From), To),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {ZoneId, _} -> validate([
            persistence:set_add(effective_parents_id(From), To),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {_, ZoneId} -> validate([
            persistence:set_add(effective_children_id(To), From),
            persistence:set(effective_edge_id(From, To), Permissions)
        ]);
        {_, _} -> {error, vertices_not_found}
    end.

-spec update_edge(From :: binary(), To :: binary(), Permissions :: permissions()) -> ok | {error, any()}.
update_edge(From, To, Permissions) ->
    post(From, {parent, updated, From, To, Permissions}),
    post(To, {child, updated, To, From, Permissions}),
    post_events_about_effective_children(From, To, updated, Permissions),
    post_events_about_effective_parents(To, From, updated, Permissions),
    validate([persistence:set(edge_id(From, To), Permissions)]).

-spec update_effective_edge(From :: binary(), To :: binary(), Permissions :: permissions()) -> ok | {error, any()}.
update_effective_edge(From, To, Permissions) -> validate([
    persistence:set(effective_edge_id(From, To), Permissions),
    persistence:set_add(effective_children_id(To), From),
    persistence:set_add(effective_parents_id(From), To)
]).


-spec remove_edge(From :: binary(), To :: binary()) -> ok | {error, any()}.
remove_edge(From, To) ->
    ZoneId = gmm_utils:zone_id(),
    FromZone = gmm_utils:owner_of(From),
    ToZone = gmm_utils:owner_of(To),
    post(To, {child, removed, To, From, nil}),
    post(From, {parent, removed, From, To, nil}),
    post_events_about_effective_children(From, To, removed, nil),
    post_events_about_effective_parents(To, From, removed, nil),
    case {FromZone, ToZone} of
        {ZoneId, ZoneId} -> validate([
            persistence:del(edge_id(From, To)),
            persistence:set_remove(children_id(To), From),
            persistence:set_remove(parents_id(From), To)
        ]);
        {ZoneId, _} -> validate([
            persistence:del(edge_id(From, To)),
            persistence:set_remove(parents_id(From), To)
        ]);
        {_, ZoneId} -> validate([
            persistence:del(edge_id(From, To)),
            persistence:set_remove(children_id(To), From)
        ]);
        {_, _} -> {error, vertices_not_found}
    end.

-spec remove_effective_edge(From :: binary(), To :: binary()) -> ok | {error, any()}.
remove_effective_edge(From, To) ->
    ZoneId = gmm_utils:zone_id(),
    FromZone = gmm_utils:owner_of(From),
    ToZone = gmm_utils:owner_of(To),
    case {FromZone, ToZone} of
        {ZoneId, ZoneId} -> validate([
            persistence:del(effective_edge_id(From, To)),
            persistence:set_remove(effective_children_id(To), From),
            persistence:set_remove(effective_parents_id(From), To)
        ]);
        {ZoneId, _} -> validate([
            persistence:del(effective_edge_id(From, To)),
            persistence:set_remove(effective_parents_id(From), To)
        ]);
        {_, ZoneId} -> validate([
            persistence:del(effective_edge_id(From, To)),
            persistence:set_remove(effective_children_id(To), From)
        ]);
        {_, _} -> {error, vertices_not_found}
    end.

-spec edge_exists(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
edge_exists(From, To) -> persistence:exists(edge_id(From, To)).

-spec effective_edge_exists(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
effective_edge_exists(From, To) -> persistence:exists(effective_edge_id(From, To)).


-spec effective_edge_exists(From :: binary(), To :: binary()) -> {ok, boolean()} | {error, any()}.
effective_edge_exists(From, To) -> persistence:exists(effective_edge_id(From, To)).


-spec get_edge(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
get_edge(From, To) ->
    do_get_edge(From, To, edge_id(From, To)).

-spec get_effective_edge(From :: binary(), To :: binary()) -> {ok, map()} | {error, any()}.
get_effective_edge(From, To) ->
    do_get_edge(From, To, effective_edge_id(From, To)).

do_get_edge(From, To, Edge) ->
    case persistence:get(Edge) of
        {error, Error} -> {error, Error};
        {ok, Permissions} ->
            {ok, #{<<"from">> => From, <<"to">> => To, <<"permissions">> => Permissions}}
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

%% @todo implement this - basically all zones that this zone has contact with
-spec all_zones() -> {ok, list(binary())} | {error, any()}.
all_zones() ->
    {ok, []}.

-spec effective_list_parents(Vertex :: binary()) -> {ok, list(binary())} | {error, any()}.
effective_list_parents(Vertex) -> persistence:set_list_members(effective_parents_id(Vertex)).

-spec effective_list_children(Vertex :: binary()) -> {ok, list(binary())} | {error, any()}.
effective_list_children(Vertex) -> persistence:set_list_members(effective_children_id(Vertex)).

post(Vertex, Events) when is_list(Events) ->
    lists:map(fun (Event) -> post(Vertex, Event) end, Events);

post(Vertex, Event) ->
    spawn(fun () -> do_post(Vertex, Event) end).

do_post(Vertex, Event) ->
    inbox:post(Vertex, #{<<"event">> => Event}).

post_events_about_effective_children(Vertex, TargetVertex, Type, Permissions) ->
    {ok, Children} = effective_list_children(Vertex),
    io:format("Children of ~p are ~p~n", [Vertex, Children]),
    Events = lists:map(fun (Child) -> {child, Type, TargetVertex, Child, Permissions} end, Children),
    post(Vertex, Events).

post_events_about_effective_parents(Vertex, TargetVertex, Type, Permissions) ->
    {ok, Parents} = effective_list_parents(Vertex),
    io:format("Parents of ~p are ~p~n", [Vertex, Parents]),
    Events = lists:map(fun (Parent) -> {parent, Type, TargetVertex, Parent, Permissions} end, Parents),
    io:format("And the events are ~p~n", [Events]),
    post(Vertex, Events).

test() ->
    A = <<"zone0/A">>,
    B = <<"zone0/B">>,
    C = <<"zone0/C">>,
    D = <<"zone0/D">>,
    E = <<"zone0/E">>,
    Perms = <<"11111">>,
    create_edge(A, B, Perms),
    create_edge(A, D, Perms),
    timer:sleep(1000),
    io:format("Next test case~n"),
    create_edge(D, C, Perms),
    timer:sleep(1000),
    io:format("Next test case~n"),
    create_edge(B, C, Perms),
    timer:sleep(1000),
    io:format("Next test case~n"),
    create_edge(E, A, Perms),
    ok.