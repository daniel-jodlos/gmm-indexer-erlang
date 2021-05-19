%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2021 20:51
%%%-------------------------------------------------------------------
-module(gmm_rest_handler).
-author("pawel").

-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
    case maps:get(handler, State) of
        vertices -> rest_handler_vertices:init(Req, State);
        edges -> rest_handler_edges:init(Req, State)
    end.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/xml">>, from_json},
        {<<"application/json">>, from_json}
    ], Req, State}.

resource_exists(Req, State) ->
    case maps:get(handler, State) of
        vertices -> rest_handler_vertices:resource_exists(Req, State);
        edges -> rest_handler_edges:resource_exists(Req, State)
    end.

%% DELETE callback
delete_resource(Req, State) ->
    case maps:get(handler, State) of
        vertices -> rest_handler_vertices:delete_resource(Req, State);
        edges -> rest_handler_edges:delete_resource(Req, State)
    end.

delete_completed(Req, State) ->
    {false, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST handler
from_json(Req, State) ->
    case maps:get(handler, State) of
        vertices -> rest_handler_vertices:from_json(Req, State);
        edges -> rest_handler_edges:from_json(Req, State)
    end.

%% GET handler
to_json(Req, State) ->
    case maps:get(handler, State) of
        vertices -> rest_handler_vertices:to_json(Req, State);
        edges -> rest_handler_edges:to_json(Req, State)
    end.
