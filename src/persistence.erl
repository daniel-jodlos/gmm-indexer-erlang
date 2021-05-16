%%%-------------------------------------------------------------------
%% @doc gmm persistence module.
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
    get_user/2, 
    get_users/1, 
    add_user/2, 
    delete_user/2, 
    update_user/3,
    add_user_to_group/2,
    is_member_of_group/2,
    list_group_users/1,
    remove_user_from_group/2
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

get(ServerName, Key) ->
    gen_server:call(ServerName, {get, Key}).

set(ServerName, Key, Value) ->
    gen_server:call(ServerName, {set, Key, Value}).

del(ServerName, Key) ->
    gen_server:call(ServerName, {del, Key}).

keys(ServerName, Pattern) ->
    gen_server:call(ServerName, {keys, Pattern}).

set_add(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_add, Key, Value}).

set_remove(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_remove, Key, Value}).

set_is_member(Key, Value) ->
    gen_server:call(?REDIS_SERVER, {set_is_member, Key, Value}).

set_list_members(Key) ->
    gen_server:call(?REDIS_SERVER, {set_list_members, Key}).

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

children_key(Key) when is_binary(Key) == true ->
    <<Key/binary, "/children">>;
children_key(Key) -> children_key(binary:list_to_bin(Key)).

add_user_to_group(User, Group) ->
    set_add(children_key(Group), User).

remove_user_from_group(User, Group) ->
    set_remove(children_key(Group), User).

is_member_of_group(User, Group) ->
    {ok, Result} = set_is_member(children_key(Group), User),
    Result.

list_group_users(Group) ->
    {ok, Result} = set_list_members(children_key(Group)),
    Result.
