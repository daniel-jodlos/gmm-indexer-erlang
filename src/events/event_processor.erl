-module(event_processor).
-author("Daniel Jodłoś").

-export([
  process/1,
  async_process/1,
  post/1
]).

-type ref() :: binary().
-type event() :: {child | parent, removed | updated, ref(), ref(), any()}. % {node_type, operation_type, target, argument, options}

-spec graph_operation(removed | updated) -> fun((ref(), ref(), any()) -> boolean()).
graph_operation(removed) ->
  fun (A, B, _Args) -> 
   case graph:effective_edge_exists(A, B) of
      {ok, true} ->
          graph:remove_effective_edge(A, B),
          true;
      _Else -> false
   end
  end;

graph_operation(updated) -> 
  fun (A,B, Permissions) ->
    case graph:get_effective_edge(A, B) of
      {ok, Permissions} -> false;
      _Else ->
        ok = graph:update_effective_edge(A, B, Permissions),
        true
    end
  end.

-spec async_process(map()) -> pid().
async_process(Event) ->
    spawn(?MODULE, process, [Event]).

-spec process(map()) -> ok | {error, any()}.
process(#{<<"event">> := Event}) ->
    instrumentation:event_started(Event),
    {_NodeType, _Type, Vertex, _Args, _Options} = Event,
    Result = do_process(Event),
    ok = inbox:free_vertex(Vertex),
    instrumentation:event_finished(Event),
    case Result of
        {ok, true, Events} ->
            lists:map(fun post/1, Events()),
            ok;
        {ok, false, _Events} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec do_process(event()) -> {ok, boolean(), fun(() -> [event()])} | {error, any()}.
do_process({child, Operation, To, Child, Args}) ->
    GraphAction = graph_operation(Operation),
    Updated = GraphAction(Child, To, Args),
    Events = fun () ->
        {ok, Parents} = graph:list_parents(To),
        lists:map(fun (Parent) -> {child, Operation, Parent, Child, Args} end, Parents)
    end,
    {ok, Updated, Events};

do_process({parent, Operation, Target, Parent, Args}) ->
    GraphAction = graph_operation(Operation),
    Updated = GraphAction(Target, Parent, Args),
    Events = fun () ->
        {ok, Children} = graph:list_children(Target),
        lists:map(fun (Child) -> {parent, Operation, Child, Parent, Args} end, Children)
    end,
    {ok, Updated, Events};

do_process(_Event) ->
    {error, "Unhandled event"}.

post({_, _, Target, _, _} = Event) ->
    EventMap = #{<<"event">> => Event},
    inbox:post(Target, EventMap),
    ok.
