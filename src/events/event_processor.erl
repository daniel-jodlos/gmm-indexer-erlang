-module(event_processor).

-export([
    process/2,
    async_process/2
]).

async_process(Vertex, Event) ->
    spawn(?MODULE, process, [Vertex, Event]).
    
process(Vertex, Event) ->
    try 
        #{<<"type">> := Type} = Event,
        case Type of
            <<"child/updated">> -> process_child_change(Vertex, Event);
            <<"child/removed">> -> process_child_removed(Vertex, Event);
            <<"parent/updated">> -> process_parent_change(Vertex, Event);
            <<"parent/removed">> -> process_parent_removed(Vertex, Event)
        end,
        inbox:free_vertex(Vertex)
    catch _:_ -> inbox:free_vertex(Vertex) end,
    ok.
    
process_child_change(Vertex, Event) ->
    #{<<"effectiveVertices">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveChildren = 
        lists:filter(fun (Child) ->
            graph:add_intermediate_vertex(Child, Vertex, Vertex, Sender),
            {ok, Updated} = graph:add_effective_child(Vertex, Child),
            recalculatePermissions(Child, Vertex),
            {ok, Result} = parser:parse_boolean(Updated),
            Result
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVertices">> => EffectiveChildren},
    {ok, Parents} = graph:list_parents(Vertex),
    propagate(Parents, NewEvent),
    ok.

process_child_removed(Vertex, Event) ->
    #{<<"effectiveVertices">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveChildren =
        lists:filter(fun (Child) ->
            graph:remove_intermediate_vertex(Child, Vertex, Vertex, Sender),
            case graph:get_intermediate_verticies(Child, Vertex, Vertex) of
                [] ->
                    graph:remove_effective_edge(Child, Vertex),
                    graph:remove_effective_child(Vertex, Child),
                    true;
                _else ->
                    recalculatePermissions(Child, Vertex),
                    false
            end
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVertices">> => EffectiveChildren},
    {ok, Parents} = graph:list_parents(Vertex),
    propagate(Parents, NewEvent),
    ok.

process_parent_change(Vertex, Event) ->
    #{<<"effectiveVertices">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveParents =
        lists:filter(fun (Parent) ->
            graph:add_intermediate_vertex(Vertex, Parent, Vertex, Sender),
            {ok, Updated} = graph:add_effective_parent(Vertex, Parent),
            {ok, Result} = parser:parse_boolean(Updated),
            Result
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVertices">> => EffectiveParents},
    {ok, Children} = graph:list_children(Vertex),
    propagate(Children, NewEvent),  
    ok.

process_parent_removed(Vertex, Event) ->
    #{<<"effectiveVertices">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveParents =
        lists:filter(fun (Parent) ->
            graph:remove_intermediate_vertex(Vertex, Parent, Vertex, Sender),
            case graph:get_intermediate_verticies(Vertex, Parent, Vertex) of
                [] ->
                    graph:remove_effective_parent(Vertex, Parent),
                    true;
                _other -> false
            end
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVertices">> => EffectiveParents},
    {ok, Children} = graph:list_children(Vertex),
    propagate(Children, NewEvent),  
    ok.
    
recalculatePermissions(Child, Vertex) ->
    Expected = calculatePermissions(Child, Vertex),
    case graph:get_effective_edge(Child, Vertex) of
        {error, _reason} ->
            graph:create_effective_edge(Child, Vertex, Expected),
            true;
        {ok, #{<<"permissions">> := Expected}} -> false;
        _else ->
            graph:update_effective_edge(Child, Vertex, Expected),
            true
    end.

calculatePermissions(From, To) ->
    Intermediate = graph:get_intermediate_verticies(From, To, To),
    IntermediatePermissions = lists:map(fun (Vertex) ->
        case graph:get_edge(Vertex, To) of
            {ok, #{<<"permissions">> := Permissions}} -> Permissions;
            _other -> <<"00000">>
        end
    end, Intermediate),
    lists:foldl(fun (A, B) -> gmm_utils:permissions_or(A, B) end, <<"00000">>, IntermediatePermissions).
    
propagate(Targets, Event) ->
    #{<<"effectiveVertices">> := Verticies} = Event,
    case Verticies of
        [] -> ok;
        _else ->
            lists:foreach(fun (Target) -> inbox:post(Target, Event) end , Targets)
    end.