%%%-------------------------------------------------------------------
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------

-module(handler_template).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    delete_resource/2,
    delete_completed/2,
    resource_exists/2
]).

-export([
    from_json/2,
    to_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

delete_resource(Req, State) ->
    {false, Req, State}.

delete_completed(Req, State) ->
    {false, Req, State}.

%% POST handler
from_json(Req, State) ->
    {false, Req, State}.

%% GET handler
to_json(Req, State) ->
    {json_utils:empty_json(), Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

