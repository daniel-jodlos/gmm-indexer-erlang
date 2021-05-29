%%%-------------------------------------------------------------------
%%% @author pawel
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2021 19:18
%%%-------------------------------------------------------------------

-author("pawel").

%%-record(state, {table = users_table}).

-define(REDIS_SERVER, redis_server).
-define(ZONE_ID, os:getenv("ZONE_ID", "zone0")).
