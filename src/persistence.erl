%%%-------------------------------------------------------------------
%% @doc gmm persistence module.
%% @end
%%%-------------------------------------------------------------------

-module(persistence).
-behaviour(gen_server).

-export([stop/1, start_link/1]).
-export([get_user/2, get_users/1, add_user/2, delete_user/2]).

%% gen_server api

% start(Name) ->
%     _sup:start_child(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, RedisClient} = eredis:start_link(),
    {ok, RedisClient}.

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

handle_call(_Request, _From, RedisClient) ->
    {reply, ok, RedisClient}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% redis api

redis_get(Name, Key) ->
    gen_server:call(Name, {get, Key}).

redis_set(Name, Key, Value) ->
    gen_server:call(Name, {set, Key, Value}).

redis_del(Name, Key) ->
    gen_server:call(Name, {del, Key}).

redis_keys(Name, Pattern) ->
    gen_server:call(Name, {keys, Pattern}).

%% persistence higher level api

get_user(Name, Id) ->
    redis_get(Name, integer_to_binary(Id)).

get_users(Name) ->
    redis_keys(Name, "*").

add_user(Name, Json) ->
    BitJson = list_to_bitstring(Json),
    MapJson = jiffy:decode(BitJson, [return_maps]),
    Id = maps:get(MapJson, <<"id">>),
    redis_set(Name, Id, Json).

delete_user(Name, Id) ->
    redis_del(Name, Id),
    ok.