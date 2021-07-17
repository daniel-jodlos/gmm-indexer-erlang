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

-spec process(event()) -> ok | {error, any()}.
process({child, Operation, To, Child, Args}) ->
  GraphAction = graph_operation(Operation),
  GraphAction(Child, To, Args);

process({parent, Operation, Target, Parent, Args}) ->
  GraphAction = graph_operation(Operation),
  GraphAction(Target, Parent, Args);

process(_Event) ->
  {error, "Unhandled event"}.
