%%%-------------------------------------------------------------------
%%% @author Svatopluk
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(client).
-author("Svatopluk").


%% API
-export([getIdList/0, getIdNumber/0, getUserInfo/1, deleteUser/1, addUser/1, test/0]).

test()->
  ok.

urlAddress() ->
  <<"localhost:8080/">>.

% 

getRequest(URL)->
  Method = get,
  Headers = [],
  Payload = <<>>,
  Options = [],
  {ok, StatusCode, RepsHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
  {ok, StatusCode, RepsHeaders, ClientRef}.

% reads body of get request
readBody(ClientRef) ->
  {ok, _} = hackney:body(ClientRef).


% Get Id List Section

getIdList() ->
  application:ensure_all_started(hackney),
  URL = urlAddress()+"users",
  {ok, _, _, ClientRef} = getRequest(URL),
  {ok, Body} = readBody(ClientRef),
  listBodyElements(binary:bin_to_list(Body)).

getIdNumber()->
  application:ensure_all_started(hackney),
  URL = urlAddress()+"users",
  {ok, _, _, ClientRef} = getRequest(URL),
  {ok, Body} = readBody(ClientRef),
  countBodyElements(binary:bin_to_list(Body), 0).

countBodyElements(Body, Acc) ->
  case Body of
    [] -> Acc;
    Body ->
      [_ | Tail] = string:split(Body, ","),
      countBodyElements(Tail, Acc + 1)
  end.

% Get User Info Section

getUserInfo(Id)->
  application:ensure_all_started(hackney),
  URL = urlAddress()+"users/"+Id,
  {ok, _, _, ClientRef} = getRequest(URL),
  {ok, Body} = readBody(ClientRef),
  binary:bin_to_list(Body).

listBodyElements(Body) ->
  case Body of
    [] -> [];
    Body ->
      [Head | Tail] = string:split(Body, ","),
      io:format("~s~n", [Head]),
      listBodyElements(Tail)
  end.

% Delete User Section
deleteRequest(URL)->
  Method = delete,
  Headers = [],
  Payload = <<>>,
  Options = [],
  {ok, StatusCode, RepsHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
  {ok, StatusCode, RepsHeaders, ClientRef}.

deleteUser(Id)->
  application:ensure_all_started(hackney),
  URL = urlAddress()+"users/"+Id,
  {ok, _, _, _} = deleteRequest(URL).


% 3) POST - dodaj uzytkownika
% curl -i -X POST localhost:8080/users --header "Content-Type: application/json" -d '{"name": "Karol"}'

% Add User Section
addUser(Name)->
  application:ensure_all_started(hackney),
  Transport = hackney_ssl,
  Host = << "localhost:8080/users" >>,
  Port = 443,
  Options = [],
  {ok, ConnRef} = hackney:connect(Transport, Host, Port, Options),
  PostBody = "{	\"name\": \""+Name+"\"}",
  ReqBody = <<PostBody>>,
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  NextPath = <<"localhost:8080/users">>,
  NextMethod = post,
  NextReq = {NextMethod, NextPath, ReqHeaders, ReqBody},
  {ok, _, _, ConnRef} = hackney:send_request(ConnRef, NextReq),
  {ok, _} = hackney:body(ConnRef).