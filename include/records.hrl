%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2021 19:18
%%%-------------------------------------------------------------------

-author("pawel").

-define(REDIS_SERVER, redis_server).
-define(ZONE_ID, os:getenv("ZONE_ID", "zone0")).

-define(REDIS_CLIENT, "REDIS_CLIENT").

-type permissions() :: <<_:5*8>>.

-type rest_handler_state() :: bad_request | map().

-type notification() :: #{
    zone            := binary(),
    time            := binary(),
    thread          := binary(),
    notif_type      := binary(),
    trace           := binary(),
    event_id        := binary(),
    event_type      := binary(),
    vertex          := binary(),
    sender          := binary(),
    original_sender := binary(),
    fork_children   := integer()
}.
