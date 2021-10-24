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
-export([replace/3, gather/2, gather_reaches/2, gather_permissions/2]).

-spec replace(T, T, [T]) -> [T].
replace(Element, Replacement, [Element | T]) ->
  [Replacement | T];
replace(Element, Replacement, [H | T]) ->
  [H | replace(Element, Replacement, T)];
replace(_Element, _Replacement, []) ->
  [].


permissions_gather_combined(AlreadyGathered, NewResult)->
  JoinPermissions = fun(A, B) -> gmm_utils:permissions_or(A,B) end,
  JoinPermissions(AlreadyGathered, NewResult).

gather_permissions(HappyEndCondition, CurrentPermission)->

    Gather = fun() ->
      receive {_, {_, Result}} ->

        case permissions_gather_combined(CurrentPermission, Result) of
          HappyEndCondition -> HappyEndCondition;
          Other -> gather_permissions(HappyEndCondition, Other)
        end

      after 5000 ->
        CurrentPermission
      end end,

    Gather().

gather_reaches(Pids, HappyEndCondition) ->

  Gather = fun F(PendingPids = [_ | _], PidsOrResults) ->
    receive {Pid, Result} ->

      case Result of
        HappyEndCondition -> HappyEndCondition;
        Other ->
          NewPidsOrResults = replace(Pid, Other, PidsOrResults),
          F(lists:delete(Pid, PendingPids), NewPidsOrResults)
      end

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

      case Errors of
        [] -> lists:all(fun(X) -> X end, AllResults);
        _ -> false
      end end,

  Gather(Pids, Pids).


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