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

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {ParsedParams, Req} =
        case {Method, maps:get(operation, State)} of
            {<<"POST">>, add} ->
                {cowboy_req:match_qs([{type, nonempty}, {name, nonempty}], Req0), Req0};
            {<<"GET">>, details} ->
                {cowboy_req:match_qs([{id, nonempty}], Req0), Req0};
            {<<"GET">>, listing} ->
                {#{}, Req0};
            {<<"POST">>, delete} ->
                {cowboy_req:match_qs([{id, nonempty}], Req0), Req0};
            {<<"POST">>, bulk} ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                {ok, List} = parse_bulk_list(Data),
                {#{body => List}, Req1}
        end,
    NewState = maps:merge(State, ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, State) ->
    Result =
        case maps:get(operation, State) of
            add ->
                Id = gmm_utils:create_vertex_id(maps:get(name, State)),
                graph:vertex_exists(Id);
            Op when Op =:= details; Op =:= delete -> graph:vertex_exists(maps:get(id, State));
            listing -> {ok, true};
            bulk -> {ok, true} %% @todo maybe change it to actually check stuff, but I don't think so
        end,
    {ok, Bool} = Result,
    {Bool, Req, State}.

%% POST handler
from_json(Req, State) ->
    ExecutionResult =
        case maps:get(operation, State) of
            add -> graph:create_vertex(maps:get(type, State), maps:get(name, State));
            delete -> graph:remove_vertex(maps:get(id, State));
            bulk -> lists:foreach(fun({Type, Name}) -> graph:create_vertex(Type, Name) end, maps:get(body, State))
        end,
    case ExecutionResult of
        ok -> {true, Req, State};
        {ok, Val} ->
            Req1 = cowboy_req:set_resp_body(gmm_utils:encode(Val), Req),
            {true, Req1, State};
        {error, _} -> {false, Req, State}
    end.
%%    RequestResult =
%%        case ExecutionResult of
%%            ok -> true;
%%            {ok, Val} -> {true, gmm_utils:encode(Val)};
%%            {error, _} -> false
%%        end,
%%    {RequestResult, Req, State}.

%% GET handler
to_json(Req, State) ->
    {ok, Result} =
        case maps:get(operation, State) of
            details -> graph:get_vertex(maps:get(id, State));
            listing -> graph:list_vertices()
        end,
    {gmm_utils:encode(Result), Req, State}.


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
