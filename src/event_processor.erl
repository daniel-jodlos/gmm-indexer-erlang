-module(event_processor).
-author("Daniel Jodłoś").

-export([
  process/1
]).

-type ref() :: {binary(), binary()}. % id, permissions
-type event() :: {child_added, child_removed, parent_added, parent_removed, ref(), ref(), [any()]}. % {type, target, argument}

-spec process(event()) -> ok | {error, any()}.
process({child_added, To, Child, [Permissions]}) ->
  graph:create_effective_edge(Child, To, Permissions);

process({child_removed, From, Child, []}) ->
  graph:remove_effective_edge(Child, From);

process({parent_added, To, Parent, [Permissions]}) ->
  graph:create_effective_edge(To, Parent, Permissions);

process({parent_removed, From, Parent, []}) ->
  graph:remove_effective_edge(From, Parent);

process(_Event) ->
  {error, "Unhandled event"}.
