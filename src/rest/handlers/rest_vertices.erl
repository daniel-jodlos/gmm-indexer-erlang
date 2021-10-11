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
    NewState = parser:parse_rest_params(Req, State, [{type, nonempty}, {name, nonempty}], []),
    {cowboy_rest, Req, NewState};
init(Req = #{method := <<"GET">>}, State = #{operation := details}) ->
    NewState = parser:parse_rest_params(Req, State, [{id, nonempty}], [{id, fun gmm_utils:validate_vertex_id/1}]),
    {cowboy_rest, Req, NewState};
init(Req = #{method := <<"GET">>}, State = #{operation := listing}) ->
    {cowboy_rest, Req, State};
init(Req = #{method := <<"POST">>}, State = #{operation := delete}) ->
    NewState = parser:parse_rest_params(Req, State, [{id, nonempty}], [{id, fun gmm_utils:validate_vertex_id/1}]),
    {cowboy_rest, Req, NewState};
init(Req0 = #{method := <<"POST">>}, State = #{operation := bulk}) ->
    {Req, NewState} = parser:parse_rest_body(Req0, State, fun parse_bulk_list/1),
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
from_json(Req, State = #{operation := bulk, body := List}) ->
    lists:foreach(fun({Type, Name}) -> {ok, _} = graph:create_vertex(Type, Name) end, List),
    {true, Req, State}.

%% GET handler
to_json(Req, State = #{operation := details, id := Id}) ->
    {ok, Details} = graph:get_vertex(Id),
    {gmm_utils:encode(Details), Req, State};
to_json(Req, State = #{operation := listing}) ->
    {ok, Vertices} = graph:list_vertices(),
    {gmm_utils:encode(Vertices), Req, State}.


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
