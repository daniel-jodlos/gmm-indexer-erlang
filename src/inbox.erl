%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that receives events
%%%  and potentially redirects them to proper outbox
%%% @end
%%%-------------------------------------------------------------------
-module(inbox).
-author("pawel").

%% API
-export([
    post/2
]).


%%%---------------------------
%% External functions
%%%---------------------------

post(Zone, Event) ->
    ThisZone = gmm_utils:zone_id(),
    case Zone of
        ThisZone -> local_post(Event);
        Other -> outbox:post(Other, Event)
    end.

%%%---------------------------
%% Internal functions
%%%---------------------------

local_post(Event) ->
    ok.
