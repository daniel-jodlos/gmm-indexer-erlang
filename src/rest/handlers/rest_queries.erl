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

-include("records.hrl").


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State = #{operation := Op, algorithm := Algo}) when Op == reaches; Op == effective_permissions ->
    ParamsSpec = [{from, nonempty}, {to, nonempty}] ++ (
        case Algo of naive -> [{jumpcount, [], <<"1">>}]; indexed -> [] end),
    ParamsParsers = [{from, fun gmm_utils:validate_vertex_id/1}, {to, fun gmm_utils:validate_vertex_id/1}] ++ (
        case Algo of
            naive -> [{jumpcount, fun(Bin) -> {ok, list_to_integer( binary_to_list(Bin) )} end}];
            indexed -> []
        end),
    NewState = gmm_utils:parse_rest_params(Req, State, ParamsSpec, ParamsParsers),
    {cowboy_rest, Req, NewState};
init(Req, State = #{operation := members, algorithm := Algo}) ->
    ParamsSpec = [{'of', nonempty}] ++ (case Algo of naive -> [{jumpcount, [], <<"1">>}]; indexed -> [] end),
    ParamsParsers = [{'of', fun gmm_utils:validate_vertex_id/1}] ++ (
        case Algo of
            naive -> [{jumpcount, fun(Bin) -> {ok, list_to_integer( binary_to_list(Bin) )} end}];
            indexed -> []
        end),
    NewState = gmm_utils:parse_rest_params(Req, State, ParamsSpec, ParamsParsers),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, State = #{operation := Operation, algorithm := Algorithm}) ->
    {ok, Result} =
        case {Operation, Algorithm} of
            {reaches, naive} ->
                #{from := From, to := To, jumpcount := JumpCount} = State,
                execute(fun reaches_naive/3, [From, To, JumpCount], <<"reaches">>);
            {reaches, indexed} ->
                #{from := From, to := To} = State,
                execute(fun reaches_indexed/2, [From, To], <<"reaches">>);
            {effective_permissions, naive} ->
                #{from := From, to := To, jumpcount := JumpCount} = State,
                execute(fun effective_permissions_naive/3, [From, To, JumpCount], <<"effectivePermissions">>);
            {effective_permissions, indexed} ->
                #{from := From, to := To} = State,
                execute(fun effective_permissions_indexed/2, [From, To], <<"effectivePermissions">>);
            {members, naive} ->
                #{'of' := Of, jumpcount := JumpCount} = State,
                execute(fun members_naive/2, [Of, JumpCount], <<"members">>);
            {members, indexed} ->
                #{'of' := Of} = State,
                execute(fun members_indexed/1, [Of], <<"members">>)
        end,
    Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Result), Req),
    {true, Req1, State}.


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
                    Fun(Arg1, Arg2);
                3 ->
                    [Arg1, Arg2, Arg3] = Args,
                    Fun(Arg1, Arg2, Arg3)
            end,
        EndTime = erlang:timestamp(),
        Duration = timer:now_diff(EndTime, StartTime),
        case Result of
            {error, Reason} -> {error, Reason};
            {ok, Value} ->
                {ok, #{<<"duration">> => gmm_utils:convert_microseconds_to_iso_8601(Duration), FieldName => Value}}
        end
    catch Class:Pattern:Stacktrace ->
        gmm_utils:log_error(Class, Pattern, Stacktrace),
        {error, {Class, Pattern}}
    end.


%%%---------------------------
%% Naive Implementations
%%%---------------------------

%% Reaches

-spec reaches_naive(binary(), binary(), integer() | undefined) -> {ok, boolean()} | {error, any()}.
reaches_naive(_, _, JumpCount) when JumpCount > 1000 ->
    {error, found_a_cycle};
reaches_naive(From, To, JumpCount) ->
    ZoneId = gmm_utils:zone_id(),
    case gmm_utils:owner_of(From) of
        ZoneId ->
            case graph:edge_exists(From, To) of
                {ok, true} -> {ok, true};
                {error, Reason} -> {error, Reason};
                _ -> reaches_naive_check_parents(From, To, JumpCount + 1)
            end;
        Other ->
            case zone_client:reaches(naive, Other, From, To, JumpCount + 1) of
                {ok, #{<<"reaches">> := Bool}} -> {ok, Bool};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec reaches_naive_check_parents(binary(), binary(), integer()) -> {ok, boolean()} | {error, any()}.
reaches_naive_check_parents(From, To, JumpCount) ->
    case graph:list_parents(From) of
        {ok, Parents} ->
            lists:foldr(
                fun
                    (_, {error, Reason}) -> {error, Reason};
                    (_, {ok, true}) -> {ok, true};
                    (Parent, _) -> reaches_naive(Parent, To, JumpCount + 1)
                end,
                {ok, false},
                Parents
            );
        {error, Reason} -> {error, Reason}
    end.

%% Effective permissions

-spec effective_permissions_naive(binary(), binary(), integer()) -> {ok, permissions()} | {error, any()}.
effective_permissions_naive(_, _, JumpCount) when JumpCount > 1000 ->
    {error, found_a_cycle};
effective_permissions_naive(From, To, JumpCount) ->
    ZoneId = gmm_utils:zone_id(),
    case gmm_utils:owner_of(From) of
        ZoneId -> effective_permissions_naive_locally(From, To, JumpCount + 1);
        Other ->
            case zone_client:effective_permissions(naive, Other, From, To, JumpCount + 1) of
                {ok, #{<<"effectivePermissions">> := Perm}} -> {ok, Perm};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec effective_permissions_naive_locally(From :: binary(), To :: binary(), JumpCount :: integer()) ->
    {ok, gmm_utils:permissions()} | {error, any()}.
effective_permissions_naive_locally(From, To, JumpCount) ->
    JoinPermissions = fun(A, B) -> gmm_utils:permissions_or(A,B) end,
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
                        case effective_permissions_naive(Parent, To, JumpCount + 1) of
                            {error, Reason} -> {error, Reason};
                            {ok, Perm} -> {ok, JoinPermissions(Perm, Acc)}
                        end
                end,
                {ok, <<"00000">>},
                Parents
            );
        {error, Reason} -> {error, Reason}
    end.

%% Members

-spec members_naive(binary(), integer()) -> {ok, list(binary())} | {error, any()}.
members_naive(_, JumpCount) when JumpCount > 1000 ->
    {error, found_a_cycle};
members_naive(Of, JumpCount) ->
    ZoneId = gmm_utils:zone_id(),
    case gmm_utils:owner_of(Of) of
        ZoneId -> members_naive_locally(Of, JumpCount + 1);
        Other ->
            case zone_client:members(naive, Other, Of, JumpCount + 1) of
                {ok, #{<<"members">> := Members}} -> {ok, Members};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec members_naive_locally(Of :: binary(), JumpCount :: integer()) -> {ok, list(binary())} | {error, any()}.
members_naive_locally(Of, JumpCount) ->
    case graph:list_children(Of) of
        {ok, Children} ->
            Res = lists:foldr(
                fun
                    (_, {error, Error}) -> {error, Error};
                    (Child, {ok, Acc}) ->
                        case members_naive(Child, JumpCount + 1) of
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


%%%---------------------------
%% Indexed Implementations
%%%---------------------------

%% @todo
-spec reaches_indexed(binary(), binary()) -> {ok, boolean()} | {error, any()}.
reaches_indexed(_From, _To) ->
    {ok, false}.

%% @todo
-spec effective_permissions_indexed(binary(), binary()) -> {ok, permissions()} | {error, any()}.
effective_permissions_indexed(_From, _To) ->
    {ok, <<"00000">>}.

%% @todo
-spec members_indexed(binary()) -> {ok, list(binary())} | {error, any()}.
members_indexed(_Of) ->
    {ok, []}.
