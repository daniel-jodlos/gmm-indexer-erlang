%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(client_requests).
-author("Piotr Świderski").


%% API
-export([post_request/1, get_delete_request/2,
         get_simple_request_body/1]).

request(Method, Url, Headers, Payload, Options )->
  {ok,_,_,_} = hackney:request(Method, Url, Headers, Payload, Options).

simple_request(Method, Url) -> request(Method, Url, [], <<>>, []).

get_simple_request_body(Url)->
  {ok,_, _, ClientRef} = simple_request(get, Url),
  hackney:body(ClientRef).

post_request(Path)->
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  hackney:request(post, Path, ReqHeaders, <<>>, []).
  % potencjalnie do przyszego uzytku // w argumentach wtedy Body
  %ok  = hackney:send_body(ClientRef, Body),
  %{ok, _Status, _Headers, ClientRef} = hackney:start_response(ClientRef),
  %hackney:body(ClientRef).

get_delete_request(Url, Method)->
  {ok,_, _, ClientRef} = hackney:request(Method, Url, [], <<>>, []),
  hackney:body(ClientRef).