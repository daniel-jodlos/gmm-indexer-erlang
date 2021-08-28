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

-define(REDIS_CLIENT, "1").
-define(REDIS_CLIENT1, "2").
-define(REDIS_CLIENT2, "3").
-define(REDIS_CLIENT3, "4").
-define(REDIS_CLIENT4, "5").
-define(REDIS_CLIENT5, "6").
-define(REDIS_CLIENT6, "7").
-define(REDIS_CLIENT7, "8").
-define(REDIS_CLIENT8, "9").
-define(REDIS_CLIENT9, "10").

-type permissions() :: <<_:5*8>>.

-type rest_handler_state() :: bad_request | map().
