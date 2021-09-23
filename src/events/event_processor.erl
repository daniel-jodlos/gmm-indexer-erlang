-module(event_processor).

-export([
    process/2,
    async_process/2
]).

async_process(Vertex, Event) ->
    spawn(?MODULE, process, [Vertex, Event]).

post(Vertex, Event) ->
    %% TODO: Do something smarter with the queuing. Instant processing should work for now though.
    inbox:post(Vertex, Event),
    ok.
    
process(Vertex, Event) ->
    instrumentation:event_started(Event),
    #{<<"type">> := Type} = Event,
    case Type of
        <<"child/updated">> -> process_child_change(Vertex, Event);
        <<"child_removed">> -> process_child_removed(Vertex, Event);
        <<"parent_change">> -> process_parent_change(Vertex, Event);
        <<"parent_removed">> -> process_parent_removed(Vertex, Event)
    end,
    instrumentation:event_finished(Event),
    ok.
    
    
process_child_change(Vertex, Event) ->
    #{<<"effectiveVerticies">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveChildren = 
        lists:filter(fun (Child) ->
            {ok, Updated} = graph:add_intermediate_vertex(Child, Vertex, Vertex, Sender),
            Recalculated = recalculatePermissions(Child, Vertex),
            Updated or Recalculated
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVerticies">> => EffectiveChildren},
    {ok, Parents} = graph:effective_list_parents(Vertex),
    propagate(Parents, NewEvent),
    ok.

process_child_removed(Vertex, Event) ->
    #{<<"effectiveVerticies">> := Verticies, <<"sender">> := Sender} = Event,
    EffectiveChildren =
        lists:filter(fun (Child) ->
            graph:remove_intermediate_vertex(Child, Vertex, Vertex, Sender),
            case graph:get_intermediate_verticies(Child, Vertex, Vertex) of
                [] ->
                    graph:remove_effective_edge(Child, Vertex),
                    true;
                _else -> recalculatePermissions(Child, Vertex)
            end
        end, Verticies),
    NewEvent = Event#{<<"sender">> => Vertex, <<"effectiveVerticies">> => EffectiveChildren},
    {ok, Parents} = graph:effective_list_parents(Vertex),
    propagate(Parents, NewEvent),
    ok.

process_parent_change(Vertex, Event) ->
    ok.

process_parent_removed(Vertex, Event) ->
    ok.
    
recalculatePermissions(Child, Vertex) ->
    Expected = calculatePermissions(Child, Vertex),
    case graph:get_effective_edge(Child, Vertex) of
        {error, _reason} ->
            graph:create_effective_edge(Child, Vertex, Expected),
            true;
        {ok, Expected} -> false;
        _else ->
            graph:update_effective_edge(Child, Vertex, Expected),
            true
    end.

calculatePermissions(From, To) ->
    Intermediate = graph:get_intermediate_verticies(From, To, To),
    IntermediatePermissions = lists:map(fun (Vertex) ->
        {ok, Permissions} = graph:get_edge(Vertex, To),
        Permissions
    end, Intermediate),
    lists:foldl(fun (A, B) -> gmm_utils:permissions_or(A, B) end, <<"11111111">>, IntermediatePermissions).
    
propagate(Targets, Event) ->
    #{<<"effectiveVerticies">> := Verticies} = Event,
    case Verticies of
        [] -> ok;
        _else -> lists:foreach(fun (Target) -> inbox:post(Target, Event) end , Targets)
    end.