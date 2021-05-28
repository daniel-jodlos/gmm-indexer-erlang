%%%-------------------------------------------------------------------
%% @doc
%%  @todo
%%  Maybe it will be the common handler for naive and indexed queries
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


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

init(Req, State) ->
    ParsedParams = case maps:get(operation, State) of
                       Op when Op =:= reaches; Op =:= effective_permissions -> cowboy_req:match_qs([], Req);
                       members -> cowboy_req:match_qs([], Req)
                   end,
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% POST handler
from_json(Req, State) ->
    {false, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

