%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2021 18:34
%%%-------------------------------------------------------------------
-module(rest_meta_info).
-author("pawel").

%% API
-export([
    init/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    allowed_methods/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {Req, ParsedParams} =
        case {maps:get(op, State), Method} of
            {health_check, <<"GET">>} -> {Req0, #{}};
            {index_ready, <<"GET">>} -> {Req0, #{}};
            {dependent_zone, <<"POST">>} ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                {Req1, #{body => Data}};
            {instrumentation, <<"GET">>} -> {Req0, #{}};
            {instrumentation, <<"PUT">>} -> {Req0, cowboy_req:match_qs([{enabled, nonempty}], Req0)};
            {indexation, <<"PUT">>} -> {Req0, cowboy_req:match_qs([{enabled, nonempty}], Req0)}
        end,
    NewState = maps:merge(maps:put(method, Method, State), ParsedParams),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

%% @todo implement case for dependent_zone
resource_exists(Req, State) ->
    Result = case maps:get(op, State) of
                 dependent_zone -> some_logic;
                 _ -> true
             end,
    {Result, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST/PUT handler

%% @todo
from_json(Req, State) ->
    Result = case maps:get(op, State) of
                 instrumentation -> true;
                 indexation -> true;
                 dependent_zone -> {true, dependentZonesDto}
             end,
    {Result, Req, State}.

%% GET handler

%% @todo
to_json(Req, State) ->
    Result = case maps:get(op, State) of
                 health_check -> true;
                 index_ready -> some_boolean;
                 instrumentation -> some_boolean
             end,
    {Result, Req, State}.
