%%%-------------------------------------------------------------------
%%% @author piotr
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2021 15:22
%%%-------------------------------------------------------------------
-module(parallel_utils).
-author("piotr").

%% API
-export([replace/3, gather/2]).

-spec replace(T, T, [T]) -> [T].
replace(Element, Replacement, [Element | T]) ->
  [Replacement | T];
replace(Element, Replacement, [H | T]) ->
  [H | replace(Element, Replacement, T)];
replace(_Element, _Replacement, []) ->
  [].

gather(ConditionsMet, Pids) ->

  Gather = fun F(PendingPids = [_ | _], PidsOrResults) ->
    receive {Pid, Result} ->
      NewPidsOrResults = replace(Pid, Result, PidsOrResults),
      F(lists:delete(Pid, PendingPids), NewPidsOrResults)
    after 5000 ->
      case lists:any(fun erlang:is_process_alive/1, PendingPids) of
        true -> F(PendingPids, PidsOrResults);
        false -> error({parallel_call_failed, {processes_dead, Pids}})
      end
    end;
    F([], AllResults) ->

      Errors = lists:filtermap(
        fun({'$pmap_error', Pid, Type, Reason, Stacktrace}) ->
          {true, {Pid, Type, Reason, Stacktrace}};
          (_) -> false
        end, AllResults),

      case ConditionsMet of
        no_conditions ->
          case Errors of
            [] -> ok;
            _ -> {error, Errors}
          end;
        conditions ->
          case Errors of
            [] -> lists:all(fun(X) -> X end, AllResults);
            _ -> false
          end
      end
  end,

  Gather(Pids, Pids).