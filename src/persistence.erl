%%%-------------------------------------------------------------------
%% @doc
%%  gmm persistence module.
%% @end
%%%-------------------------------------------------------------------

-module(persistence).
-behaviour(gen_server).

-include("records.hrl").

-export([
    stop/1, 
    start_link/1, 
    init/1, 
    handle_call/3, 
    handle_info/2, 
    handle_cast/2, 
    terminate/2, 
    code_change/3
]).

-export([
    get/1,
    set/2,
    del/1,
    keys/1,
    exists/1,
    set_add/2,
    set_remove/2,
    set_is_member/2,
    set_list_members/1
]).

%% gen_server api

stop(ServerName) ->
    gen_server:call(ServerName, stop).

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

init(_Args) ->
    eredis:start_link().

handle_call(stop, _From, RedisClient) ->
    {stop, normal, stopped, RedisClient};

handle_call({get, Key}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["GET", Key]),
    {reply, Reply, RedisClient};

handle_call({set, Key, Value}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["SET", Key, Value]),
    {reply, Reply, RedisClient};

handle_call({del, Key}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["DEL", Key]),
    {reply, Reply, RedisClient};

handle_call({keys, Pattern}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["KEYS", Pattern]),
    {reply, Reply, RedisClient};

handle_call({set_add, Key, Value}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["SADD", Key, Value]),
    {reply, Reply, RedisClient};

handle_call({set_remove, Key, Value}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["SREM", Key, Value]),
    {reply, Reply, RedisClient};

handle_call({set_is_member, Key, Value}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["SISMEMBER", Key, Value]),
    {reply, Reply, RedisClient};

handle_call({set_list_members, Key}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["SMEMBERS", Key]),
    {reply, Reply, RedisClient};

handle_call({exists, Key}, _From, RedisClient) ->
    Reply = eredis:q(RedisClient, ["EXISTS", Key]),
    {reply, Reply, RedisClient};

handle_call(_Request, _From, RedisClient) ->
    {reply, unknown, RedisClient}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% redis api

get(Key) ->
    gen_server:call(?REDIS_SERVER, {get, Key}).

set(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set, Key, Value}).

del(Key) ->
    gen_server:call(?REDIS_SERVER, {del, Key}).

keys(Pattern) ->
    gen_server:call(?REDIS_SERVER, {keys, Pattern}).

exists(Key) ->
    case gen_server:call(?REDIS_SERVER, {exists, Key}) of
        {ok, <<"0">>} -> {ok, false};
        {ok, <<"1">>} -> {ok, true};
        {error, Reason} -> {error, Reason}
    end.

set_add(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_add, Key, Value}).

set_remove(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_remove, Key, Value}).

set_is_member(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_is_member, Key, Value}).

set_list_members(Key) ->
    gen_server:call(?REDIS_SERVER, {set_list_members, Key}).
