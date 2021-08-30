%%%-------------------------------------------------------------------
%% @doc
%%  gmm persistence module.
%% @end
%%%-------------------------------------------------------------------

-module(persistence).

-include("records.hrl").

-export(
  [
    create_redis_client/0
  ]
).
-export(
  [
    get/1,
    set/2,
    del/1,
    keys/1,
    exists/1,
    set_add/2,
    set_remove/2,
    set_is_member/2,
    set_list_members/1
  ]
).

%% gen_server api
% create_redis_client(N)->
%   Client = eredis:start_link(
%    [
%      {host, os:getenv("GMM_REDIS_HOST", "localhost")},
%      {port, list_to_integer(os:getenv("GMM_REDIS_PORT", "6379"))}
%    ]),
%  case Client of
%    {ok, ClientRef} ->
%      os:putenv(?REDIS_CLIENT ++ integer_to_list(N), pid_to_list(ClientRef)), Client;
%    _ -> Client
%  end.

% create_n_redis_clients(Acc, N) ->
%  case N of
%    0 -> Acc;
%    _ -> create_n_redis_clients([create_redis_client(N) | Acc], N - 1)
%  end.

create_redis_client() ->
  Client = eredis:start_link(
    [
      {host, os:getenv("GMM_REDIS_HOST", "localhost")},
      {port, list_to_integer(os:getenv("GMM_REDIS_PORT", "6379"))}
    ]),
  case Client of
    {ok, ClientRef} ->
      os:putenv(?REDIS_CLIENT, pid_to_list(ClientRef)), Client;
    _ -> Client
  end.

%% redis api


get(Key) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["GET", Key]).

set(Key, Value) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["SET", Key, Value]).

del(Key) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["DEL", Key]).

keys(Pattern) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["KEYS", Pattern]).

exists(Key) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  case eredis:q(list_to_pid(RedisClient), ["EXISTS", Key]) of
    {ok, <<"0">>} -> {ok, false};
    {ok, <<"1">>} -> {ok, true};
    {error, Reason} -> {error, Reason}
  end.


set_add(Key, Value) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["SADD", Key, Value]).

set_remove(Key, Value) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["SREM", Key, Value]).


set_is_member(Key, Value) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["SISMEMBER", Key, Value]).


set_list_members(Key) ->
  RedisClient = os:getenv(?REDIS_CLIENT),
  eredis:q(list_to_pid(RedisClient), ["SMEMBERS", Key]).