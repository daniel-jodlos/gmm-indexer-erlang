%%%-------------------------------------------------------------------
%% @doc
%%  Common handler for naive and indexed queries about graph
%% @end
%%%-------------------------------------------------------------------

-module(rest_queries).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    resource_exists/2
]).

-export([
    from_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    ParsedParams =
        case maps:get(operation, State) of
            Op when Op =:= reaches; Op =:= effective_permissions ->
                cowboy_req:match_qs([{from, nonempty}, {to, nonempty}], Req);
            members -> cowboy_req:match_qs([{'of', nonempty}], Req)
        end,
    NewState = maps:merge(State, ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, State) ->
    #{operation := Operation, algorithm := Algorithm} = State,
    ExecutionResult =
        case {Operation, Algorithm} of
            {reaches, naive} ->
                execute(fun reaches_naive/2, [maps:get(from, State), maps:get(to, State)], <<"reaches">>);
            {reaches, indexed} ->
                execute(fun reaches_indexed/2, [maps:get(from, State), maps:get(to, State)], <<"reaches">>);
            {effective_permissions, naive} ->
                execute(fun effective_permissions_naive/2,
                    [maps:get(from, State), maps:get(to, State)], <<"effectivePermissions">>);
            {effective_permissions, indexed} ->
                execute(fun effective_permissions_indexed/2,
                    [maps:get(from, State), maps:get(to, State)], <<"effectivePermissions">>);
            {members, naive} ->
                execute(fun members_naive/1, [maps:get('of', State)], <<"members">>);
            {members, indexed} ->
                execute(fun members_indexed/1, [maps:get('of', State)], <<"members">>)
        end,
    RequestResult =
        case ExecutionResult of
            {ok, Map} -> {true, gmm_utils:encode(Map)};
            {error, _} -> false
        end,
    {RequestResult, Req, State}.


%%%---------------------------
%% Executor
%%%---------------------------

-spec execute(fun((...) -> {ok, any()} | {error, any()}), list(), binary()) -> {ok, map()} | {error, any()}.
execute(Fun, Args, FieldName) ->
    try
        StartTime = erlang:timestamp(),
        Result =
            case length(Args) of
                1 ->
                    [Arg] = Args,
                    Fun(Arg);
                2 ->
                    [Arg1, Arg2] = Args,
                    Fun(Arg1, Arg2)
            end,
        EndTime = erlang:timestamp(),
        Duration = timer:now_diff(EndTime, StartTime),
        case Result of
            {error, Reason} -> {error, Reason};
            {ok, Value} ->
                {ok, #{<<"duration">> => gmm_utils:convert_microseconds_to_iso_8601(Duration), FieldName => Value}}
        end
    catch _:_ ->
        {error, "Execution error - probably wrong number of arguments"}
    end.


%%%---------------------------
%% Implementations
%%%---------------------------

%% @todo
-spec reaches_naive(binary(), binary()) -> {ok, boolean()} | {error, any()}.
reaches_naive(From, To) ->
    case members_naive(To) of
        {error, Error} -> {error, Error};
        {ok, EffChildren} -> {ok, lists:member(From, EffChildren)}
    end.

%% @todo
-spec reaches_indexed(binary(), binary()) -> {ok, boolean()} | {error, any()}.
reaches_indexed(_From, _To) ->
    {ok, false}.

%% @todo
-spec effective_permissions_naive(binary(), binary()) -> {ok, binary()} | {error, any()}.
effective_permissions_naive(From, To) ->
    ZoneId = gmm_utils:zone_id(),
    case gmm_utils:owner_of(From) of
        ZoneId -> effective_permissions_naive_locally(From, To);
        Other ->
            case zone_client:effective_permissions(naive, Other, From, To) of
                {ok, #{<<"effectivePermissions">> := Perm}} -> {ok, Perm};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec effective_permissions_naive_locally(From :: binary(), To :: binary()) -> {ok, binary()} | {error, any()}.
effective_permissions_naive_locally(From, To) ->
    JoinPermissions = fun
%%        (<<"no-route">>, Permissions) -> Permissions;
%%        (Permissions, <<"no-route">>) -> Permissions;
        (A, B) -> gmm_utils:permissions_or(A,B)
    end,
    case graph:list_parents(From) of
        {ok, Parents} ->
            lists:foldr(
                fun
                    (_, {error, Reason}) -> {error, Reason};
                    (To, {ok, Acc}) ->
                        case graph:get_edge(From, To) of
                            {error, Reason} -> {error, Reason};
                            {ok, #{<<"permissions">> := Perm}} -> {ok, JoinPermissions(Perm, Acc)}
                        end;
                    (Parent, {ok, Acc}) ->
                        case effective_permissions_naive(Parent, To) of
                            {error, Reason} -> {error, Reason};
                            {ok, Perm} -> {ok, JoinPermissions(Perm, Acc)}
                        end
                end,
                {ok, <<"00000">>},
                Parents
            );
        {error, Reason} -> {error, Reason}
    end.


%% @todo
-spec effective_permissions_indexed(binary(), binary()) -> {ok, binary()} | {error, any()}.
effective_permissions_indexed(_From, _To) ->
    {ok, <<"">>}.

%% @todo
-spec members_naive(binary()) -> {ok, list(binary())} | {error, any()}.
members_naive(Of) ->
    ZoneId = gmm_utils:zone_id(),
    case gmm_utils:owner_of(Of) of
        ZoneId -> members_naive_locally(Of);
        Other ->
            case zone_client:members(naive, Other, Of) of
                {ok, #{<<"members">> := Members}} -> {ok, Members};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec members_naive_locally(Of :: binary()) -> {ok, list(binary())} | {error, any()}.
members_naive_locally(Of) ->
    case graph:list_children(Of) of
        {ok, Children} ->
            Res = lists:foldr(
                fun
                    (_, {error, Error}) -> {error, Error};
                    (Child, {ok, Acc}) ->
                        case members_naive(Child) of
                            {ok, Result} -> {ok, sets:union(sets:from_list(Result), Acc)};
                            A -> A
                        end
                end,
                {ok, sets:from_list(Children)},
                Children),
            case Res of
                {ok, EffChildrenSet} -> {ok, sets:to_list(EffChildrenSet)};
                {error, Error} -> {error, Error}
            end;
        {error, Error} -> {error, Error}
    end.

%% @todo
-spec members_indexed(binary()) -> {ok, list(binary())} | {error, any()}.
members_indexed(_Of) ->
    {ok, []}.
