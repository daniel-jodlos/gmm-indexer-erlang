-module(event_processor).
-author("Daniel Jodłoś").

-export([
  process/1
]).

-type ref() :: {binary(), binary()}. % id, permissions
-type event() :: {child | parent, added | removed | updated, ref(), ref(), any()}. % {node_type, operation_type, target, argument, options}

graph_operation(added) -> fun graph:create_effective_edge/3;
graph_operation(removed) -> fun ({A, B, _Args}) -> graph:remove_effective_edge(A,B) end;
graph_operation(updated) -> fun graph:update_effective_edge/3.

-spec process(map()) -> ok | {error, any()}.
process(#{<<"event">> := Event}) ->
  case do_process(Event) of
      {ok, Events} ->
          lists:map(fun post/1, Events),
          ok;
      Error -> Error
  end.

-spec do_process(event()) -> {ok, [event()]} | {error, any()}.
do_process({child, Operation, To, Child, Args}) ->
    GraphAction = graph_operation(Operation),
    GraphAction(Child, To, Args),
    {ok, Parents} = graph:list_parents(To),
    Events = lists:map(fun (Parent) -> {child, Operation, Parent, Child, Args} end, Parents),
    {ok, Events};

do_process({parent, Operation, Target, Parent, Args}) ->
    GraphAction = graph_operation(Operation),
    GraphAction(Target, Parent, Args),
    {ok, Children} = graph:list_children(Target),
    Events = lists:map(fun (Child) -> {parent, Operation, Child, Parent, Args} end, Children),
    {ok, Events};

do_process(_Event) ->
    {error, "Unhandled event"}.

post({_, _, Target, _, _} = Event) ->
    EventMap = #{<<"event">> => Event}, % No tyle że w sumie nie wiem jaka to ma być mapa, ale dialyzerowi pasuje
    inbox:post(Target, EventMap),
    ok.
