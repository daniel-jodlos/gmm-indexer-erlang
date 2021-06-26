%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(requests).
-author("Piotr Świderski").


%% API
-export([post_request/1, get_delete_request/2,
         get_simple_request_body/1, post_request_with_response_body/1]).

request(Method, Url, Headers, Payload, Options )->
  application:ensure_all_started(hackney),
  {ok,_,_,_} = hackney:request(Method, Url, Headers, Payload, Options).

simple_request(Method, Url) -> request(Method, Url, [], <<>>, []).

get_simple_request_body(Url)->
  application:ensure_all_started(hackney),
  {ok,_, _, ClientRef} = simple_request(get, Url),
  hackney:body(ClientRef).


response_body(Result)->
  [Head | Tail] = Result,
  {Key, Value} = Head,
  case "location" -- binary:bin_to_list(Key) of
    [] -> Value;
    _ -> response_body(Tail)
  end.

post_request_with_response_body(Path)->
  application:ensure_all_started(hackney),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, ResultNumber, Body, _} = hackney:request(post, Path, ReqHeaders, <<>>, []),
  if ResultNumber < 400 ->
      {ResultNumber, response_body(Body)};
    ResultNumber > 399 ->
      {ResultNumber, "Error Occured"}
  end.

post_request(Path)->
  application:ensure_all_started(hackney),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  hackney:request(post, Path, ReqHeaders, <<>>, []).

get_delete_request(Url, Method)->
  application:ensure_all_started(hackney),
  {ok,_, _, ClientRef} = hackney:request(Method, Url, [], <<>>, []),
  hackney:body(ClientRef).