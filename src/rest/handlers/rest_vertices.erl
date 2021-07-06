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
    content_types_accepted/2,
    content_types_provided/2,
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

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.


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
    {ok, Id} = graph:create_vertex(Type, Name),
    Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Id), Req),
    {true, Req1, State};
from_json(Req, State = #{operation := delete, id := Id}) ->
    ok = graph:remove_vertex(Id),
    {true, Req, State};
from_json(Req, State = #{operation := bulk, body := List}) ->
    lists:foreach(fun({Type, Name}) -> ok = graph:create_vertex(Type, Name) end, List),
    {true, Req, State}.
%%from_json(Req, State) ->
%%    ExecutionResult =
%%        case maps:get(operation, State) of
%%            add -> graph:create_vertex(maps:get(type, State), maps:get(name, State));
%%            delete -> graph:remove_vertex(maps:get(id, State));
%%            bulk -> lists:foreach(fun({Type, Name}) -> graph:create_vertex(Type, Name) end, maps:get(body, State))
%%        end,
%%    case ExecutionResult of
%%        ok ->
%%            {true, Req, State};
%%        {ok, Val} ->
%%            Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Val), Req),
%%            {true, Req1, State}
%%    end.

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
        [Type, Name] when Type =:= <<"user">>; Type =:= <<"group">>; Type =:= <<"space">>; Type =:= <<"provider">> ->
            {ok, {Type, Name}};
        _ -> {error, "Invalid vertex data"}
    end;
parse_vertex_data(_) ->
    {error, "Argument is not a binary"}.

-spec parse_bulk_list(binary()) -> {ok, list({binary(), binary()})} | {error, any()}.
parse_bulk_list(Data) ->
    case gmm_utils:decode(Data) of
        #{<<"vertices">> := List} when is_list(List) ->
            ParsedList = lists:map(fun parse_vertex_data/1, List),
            case lists:all(fun({ok, _}) -> true; (_) -> false end, ParsedList) of
                true ->
                    {_, Unzipped} = lists:unzip(ParsedList),
                    {ok, Unzipped};
                false -> {error, "Incorrect JSON"}
            end;
        _ -> {error, "Incorrect JSON"}
    end.
