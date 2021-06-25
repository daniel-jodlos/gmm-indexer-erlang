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
    request/3,
    request/4,

    get_request/2,
    delete_request/1,
    post_request/2,
    post_request/3
]).


%%%---------------------------
%% Implementations
%%%---------------------------

%% generic requests

-spec request(Method :: atom(), Url :: binary(), Body :: binary(), GetResponse :: boolean()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Url, Body, GetResponse) ->
    application:ensure_all_started(hackney),
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, Url, ReqHeaders, Body) of
        {ok, SCode, _, ConnRef} when SCode < 400 ->
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
    request(Method, Url, <<>>, GetResponse).

%% specific requests

-spec get_request(Url :: binary(), GetResponse :: boolean()) -> ok | {ok, any()} | {error, any()}.
get_request(Url, GetResponse) ->
    request(get, Url, GetResponse).

-spec delete_request(Url :: binary()) -> ok | {error, any()}.
delete_request(Url) ->
    request(delete, Url, false).

-spec post_request(Url :: binary(), GetResponse :: boolean()) -> ok | {ok, any()} | {error, any()}.
post_request(Url, GetResponse) ->
    request(post, Url, GetResponse).

-spec post_request(Url :: binary(), Body :: binary(), GetResponse :: boolean()) ->
    ok | {ok, any()} | {error, any()}.
post_request(Url, Body, GetResponse) ->
    request(post, Url, Body, GetResponse).
