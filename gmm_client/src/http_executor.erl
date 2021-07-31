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
            _ -> client_utils:encode(RawBody)
        end,
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, Url, ReqHeaders, Body) of
        {ok, SCode, _, ConnRef} when (SCode div 100) == 2 ->
            case GetResponse of
                true ->
                    case hackney:body(ConnRef) of
                        {ok, Bin} -> {ok, client_utils:decode(Bin)};
                        {error, R1} -> {error, R1}
                    end;
                false -> ok
            end;
        {ok, SCode, _, _} -> {error, SCode};
        {error, R2} -> {error, R2}
    end.

-spec request(Method :: atom(), Url :: binary(), GetResponse :: boolean()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Url, GetResponse) ->
    request(Method, Url, undefined, GetResponse).
