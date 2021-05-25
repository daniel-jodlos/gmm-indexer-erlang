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

init(_Req, #{handler := unimplemented}) ->
    erlang:error("Unimplemented handler\n");
init(Req, State) ->
    (maps:get(handler, State)):init(Req, State).

allowed_methods(Req, State) ->
    case maps:get(handler, State) of
        Handler when Handler =:= some_module -> Handler:allowed_methods(Req, State);
        _ -> {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}
    end.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

resource_exists(Req, State) ->
    (maps:get(handler, State)):resource_exists(Req, State).


%% DELETE callback
delete_resource(Req, State) ->
    (maps:get(handler, State)):delete_resource(Req, State).

delete_completed(Req, State) ->
    {false, Req, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% POST handler
from_json(Req, State) ->
    (maps:get(handler, State)):from_json(Req, State).

%% GET handler
to_json(Req, State) ->
    (maps:get(handler, State)):to_json(Req, State).
