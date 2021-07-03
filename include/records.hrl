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

-type permissions() :: <<_:5*8>>.
