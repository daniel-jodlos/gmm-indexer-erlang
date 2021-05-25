%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2021 21:49
%%%-------------------------------------------------------------------
-module(rest_subhandler_template).
-author("pawel").

%% API
-export([
    init/2,
    resource_exists/2,
    delete_resource/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
    {cowboy_rest, Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    {false, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST handler

from_json(Req, State) ->
    {false, Req, State}.

%% GET handler

to_json(Req, State) ->
    {undefined, Req, State}.
