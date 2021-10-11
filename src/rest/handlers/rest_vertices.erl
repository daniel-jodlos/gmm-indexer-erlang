%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for manipulating vertices
%% @end
%%%-------------------------------------------------------------------

-module(rest_vertices).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    allow_missing_post/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_conflict/2,
    resource_exists/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req = #{method := <<"POST">>}, State = #{operation := add}) ->
    NewState = gmm_utils:parse_rest_params(Req, State, [{type, nonempty}, {name, nonempty}], []),
    {cowboy_rest, Req, NewState};
init(Req = #{method := <<"GET">>}, State = #{operation := details}) ->
    NewState = gmm_utils:parse_rest_params(Req, State, [{id, nonempty}], [{id, fun gmm_utils:validate_vertex_id/1}]),
    {cowboy_rest, Req, NewState};
init(Req = #{method := <<"GET">>}, State = #{operation := listing}) ->
    {cowboy_rest, Req, State};
init(Req = #{method := <<"POST">>}, State = #{operation := delete}) ->
    NewState = gmm_utils:parse_rest_params(Req, State, [{id, nonempty}], [{id, fun gmm_utils:validate_vertex_id/1}]),
    {cowboy_rest, Req, NewState};
init(Req0 = #{method := <<"POST">>}, State = #{operation := bulk}) ->
    {Req, NewState} = gmm_utils:parse_rest_body(Req0, State, fun parse_bulk_list/1),
    {cowboy_rest, Req, NewState};
init(Req, _) ->
    {cowboy_rest, Req, bad_request}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

allow_missing_post(Req, State = #{operation := delete}) ->
    {false, Req, State};
allow_missing_post(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

is_conflict(Req, State = #{operation := Op}) when Op == add; Op == bulk ->
    {true, Req, State};
is_conflict(Req, State) ->
    {false, Req, State}.

resource_exists(Req, bad_request) ->
    {false, Req, bad_request};
resource_exists(Req, State = #{operation := add, name := Name}) ->
    {ok, Bool} = graph:vertex_exists( gmm_utils:create_vertex_id(Name) ),
    {Bool, Req, State};
resource_exists(Req, State = #{operation := Op, id := Id}) when Op == details; Op == delete ->
    {ok, Bool} = graph:vertex_exists(Id),
    {Bool, Req, State};
resource_exists(Req, State = #{operation := listing}) ->
    {true, Req, State};
resource_exists(Req, State = #{operation := bulk, body := Body}) ->
    Bool = lists:any(
        fun({_, Name}) ->
            {ok, Bool} = graph:vertex_exists( gmm_utils:create_vertex_id(Name) ),
            Bool
        end, Body),
    {Bool, Req, State}.


%% POST handler
from_json(Req, bad_request) ->
    {false, Req, bad_request};
from_json(Req, State = #{operation := add, type := Type, name := Name}) ->
    {ok, _} = graph:create_vertex(Type, Name),
    {true, Req, State};
from_json(Req, State = #{operation := delete, id := Id}) ->
    ok = graph:remove_vertex(Id),
    {true, Req, State};
%from_json(Req, State = #{operation := bulk, body := List}) ->
%    lists:foreach(fun({Type, Name}) -> {ok, _} = graph:create_vertex(Type, Name) end, List),
%    {true, Req, State}.
from_json(Req, State = #{operation := bulk, body := List}) ->
    ok = modify_state_bulk(List),
    {true, Req, State}.
%% GET handler
to_json(Req, State = #{operation := details, id := Id}) ->
    {ok, Details} = graph:get_vertex(Id),
    {gmm_utils:encode(Details), Req, State};
to_json(Req, State = #{operation := listing}) ->
    {ok, Vertices} = graph:list_vertices(),
    {gmm_utils:encode(Vertices), Req, State}.

%%% BULK handler

-spec modify_state_bulk(Vertices :: list({binary(), binary()})) -> ok | {error, any()}.
modify_state_bulk(Vertices)->
    Parent = self(),

    Pids = lists:map(
        fun({Type, Name}) ->
            spawn(fun() ->
                Result = try graph:create_vertex(Type, Name)
                        catch Type:Reason:Stacktrace -> {'$pmap_error', self(), Type, Reason, Stacktrace} end,

                Parent ! {self(), Result}
            end)
        end, Vertices),

    % GATHERING RESULTS
    Gather = fun F(PendingPids = [_ | _], PidsOrResults) ->
        receive
            {Pid, Result} ->
                NewPidsOrResults = rest_utils:replace(Pid, Result, PidsOrResults),
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
                    (_) -> false end, AllResults),
            case Errors of
                [] -> ok;
                _ -> {error, Errors}
            end
        end,
    Gather(Pids, Pids).
%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_vertex_data(binary()) -> {ok, {binary(), binary()}} | {error, any()}.
parse_vertex_data(Bin) when is_binary(Bin) ->
    case gmm_utils:split_bin(Bin) of
        [Type, Name] ->
            case list_to_binary(string:to_lower( binary_to_list(Type) )) of
                T when T =:= <<"user">>; T =:= <<"group">>; T =:= <<"space">>; T =:= <<"provider">> ->
                    {ok, {T, Name}};
                _ -> {error, {unrecognized_type, {Type, Name}}}
            end;
        _ -> {error, {invalid_vertex_data, Bin}}
    end;
parse_vertex_data(X) ->
    {error, {invalid_vertex_data, X}}.

-spec parse_bulk_list(binary()) -> {ok, list({binary(), binary()})} | {error, any()}.
parse_bulk_list(Data) ->
    case gmm_utils:decode(Data) of
        #{<<"vertices">> := List} when is_list(List) ->
            lists:foldl(
                fun
                    ({ok, Elem}, {ok, Acc})         -> {ok, [Elem | Acc]};
                    ({error, R}, {ok, _})           -> {error, [R]};
                    ({ok, _},    {error, ErrList})  -> {error, ErrList};
                    ({error, R}, {error, ErrList})  -> {error, [R | ErrList]}
                end, {ok, []}, lists:map(fun parse_vertex_data/1, List)
            );
        _ -> {error, {invalid_bulk_json, Data}}
    end.
