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

-define(CLIENT_NUMBER, 7).

-define(CSV_FILE, "/instrumentation.csv").

-define(MAX_TIMEOUT, 120000).

-type permissions() :: <<_:5*8>>.

-type rest_handler_state() :: bad_request | map().

-type event() :: map(
    % <<"id">>                  := binary(),
    % <<"trace">>               := binary(),
    % <<"type">>                := binary(),
    % <<"sender">>              := binary(),
    % <<"originalSender">>      := binary(),
    % <<"effectiveVertices">>   := list(binary())
).

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
