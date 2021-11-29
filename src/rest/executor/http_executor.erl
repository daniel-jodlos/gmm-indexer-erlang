%%%-------------------------------------------------------------------
%%% @doc
%%%  Executes http requests for given URL
%%%  Validates Status Code of the response
%%%  Returns raw response body, it there is one
%%% @end
%%%-------------------------------------------------------------------
-module(http_executor).
-author("pawel").

%% API
-export([
    post/2,
    post/3,
    get/1,
    put/2
]).

-include("records.hrl").

%%%---------------------------
%% Implementations
%%%---------------------------

%% Specific requests

post(Url, GetResponse) ->
    request(post, Url, GetResponse).

post(Url, Body, GetResponse) ->
    request(post, Url, Body, GetResponse).

get(Url) ->
    request(get, Url, true).

put(Url, Body) ->
    request(put, Url, Body, false).


%% Generic requests

-spec request(Method :: atom(), Url :: binary(), RawBody :: undefined | any(), GetResponse :: boolean()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Url, RawBody, GetResponse) ->
    application:ensure_all_started(hackney),
    Body =
        case RawBody of
            undefined -> <<>>;
            _ -> gmm_utils:encode(RawBody)
        end,
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, Url, ReqHeaders, Body, [{pool, false}, {recv_timeout, ?MAX_TIMEOUT}]) of
        {ok, SCode, _, ConnRef} when (SCode div 100) == 2 ->
            case GetResponse of
                true ->
                    case hackney:body(ConnRef) of
                        {ok, Bin} -> {ok, gmm_utils:decode(Bin)};
                        {error, R1} -> {error, #{reason => R1, req => {Method, Url, RawBody, GetResponse}}}
                    end;
                false -> ok
            end;
        {ok, SCode, RespHeaders, _} ->
            {error, #{code => SCode, req => {Method, Url, RawBody, GetResponse}, response_headers => RespHeaders}};
        {error, R2} -> {error, #{reason => R2, req => {Method, Url, RawBody, GetResponse}}}
    end.

-spec request(Method :: atom(), Url :: binary(), GetResponse :: boolean()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Url, GetResponse) ->
    request(Method, Url, undefined, GetResponse).
