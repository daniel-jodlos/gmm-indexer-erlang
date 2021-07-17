-module(event_processor).
-author("Daniel Jodłoś").

-export([
  process/1
]).

-type ref() :: {binary(), binary()}. % id, permissions
-type event() :: {child | parent, added | removed | updated, ref(), ref(), [any()]}. % {type, target, argument}

graph_operation(added) -> fun graph:create_effective_edge/3;
graph_operation(removed) -> fun ({A, B, _Args}) -> graph:remove_effective_edge(A,B) end;
graph_operation(updated) -> fun graph:update_effective_edge/3.

-spec process(event()) -> {ok, [event()]} | {error, any()}.
process({child, Operation, To, Child, Args}) ->
  GraphAction = graph_operation(Operation),
  GraphAction(Child, To, Args),
  Parents = graph:list_parents(To),
  Events = lists:map(fun (Parent) -> {child, Operation, Parent, Child, Args} end, Parents),
  {ok, Events};

process({parent, Operation, Target, Parent, Args}) ->
  GraphAction = graph_operation(Operation),
  GraphAction(Target, Parent, Args),
  Children = graph:list_children(Target),
  Events = lists:map(fun (Child) -> {parent, Operation, Child, Parent, Args} end, Children),
  {ok, Events};

process(_Event) ->
  {error, "Unhandled event"}.
