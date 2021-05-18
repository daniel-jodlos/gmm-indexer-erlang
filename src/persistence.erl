%%%-------------------------------------------------------------------
%% @doc gmm persistence module.
%% @end
%%%-------------------------------------------------------------------

-module(persistence).
-behaviour(gen_server).

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
    get_user/2, 
    get_users/1, 
    add_user/2, 
    delete_user/2, 
    update_user/3
]).

%% gen_server api

stop(ServerName) ->
    gen_server:call(ServerName, stop).

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

init(_Args) ->
    eredis:start_link([{host, os:getenv("GMM_REDIS_HOST", "localhost")}, {port, list_to_integer(os:getenv("GMM_REDIS_PORT", 6379))}]).

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

get(ServerName, Key) ->
    gen_server:call(ServerName, {get, Key}).

set(ServerName, Key, Value) ->
    gen_server:call(ServerName, {set, Key, Value}).

del(ServerName, Key) ->
    gen_server:call(ServerName, {del, Key}).

keys(ServerName, Pattern) ->
    gen_server:call(ServerName, {keys, Pattern}).

%% persistence higher level api

get_user(ServerName, Id) ->
    get(ServerName, Id).

get_users(ServerName) ->
    keys(ServerName, "*").

add_user(ServerName, Name) ->
    Id = << <<Y>> ||<<X:4>> <= crypto:hash(md5, term_to_binary(make_ref())), Y <- integer_to_list(X,16)>>,
    Json = json_utils:encode(#{<<"id">> => Id, <<"Name">> => Name}),
    Response = set(ServerName, Id, Json),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> {ok, Id}
    end.

update_user(ServerName, Id, NewName) ->
    Json = json_utils:encode(#{<<"id">> => Id, <<"Name">> => NewName}),
    Response = set(ServerName, Id, Json),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.

delete_user(ServerName, Id) ->
    Response = del(ServerName, Id),
    case Response of
        {error, Reason} -> {error, Reason};
        {ok, _Result} -> ok
    end.