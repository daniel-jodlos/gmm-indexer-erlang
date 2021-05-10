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
-export([get_id_list/0, get_id_number/0, get_user_info/1, delete_user/1, add_user/1, test/0]).

test()->
  ok.

url_address() ->
  "localhost:8080/".

% 

get_request(URL)->
  Method = get,
  Headers = [],
  Payload = <<>>,
  Options = [],
  {ok, StatusCode, RepsHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
  {ok, StatusCode, RepsHeaders, ClientRef}.

% reads body of get request
read_body(ClientRef) ->
  {ok, _} = hackney:body(ClientRef).


% Get Id List Section

get_id_list() ->
  application:ensure_all_started(hackney),
  URL = url_address()++"users",
  {ok, _, _, ClientRef} = get_request(URL),
  {ok, Body} = read_body(ClientRef),
  list_body_elements(binary:bin_to_list(Body)).

get_id_number()->
  application:ensure_all_started(hackney),
  URL = url_address()++"users",
  {ok, _, _, ClientRef} = get_request(URL),
  {ok, Body} = read_body(ClientRef),
  count_body_elements(binary:bin_to_list(Body), 0).

count_body_elements(Body, Acc) ->
  case Body of
    [] -> Acc;
    Body ->
      [_ | Tail] = string:split(Body, ","),
      count_body_elements(Tail, Acc + 1)
  end.

% Get User Info Section

get_user_info(Id)->
  application:ensure_all_started(hackney),
  URL = url_address()++"users/"++Id,
  {ok, _, _, ClientRef} = get_request(URL),
  {ok, Body} = read_body(ClientRef),
  binary:bin_to_list(Body).

list_body_elements(Body) ->
  case Body of
    [] -> [];
    Body ->
      [Head | Tail] = string:split(Body, ","),
      io:format("~s~n", [Head]),
      list_body_elements(Tail)
  end.

% Delete User Section
delete_request(URL)->
  Method = delete,
  Headers = [],
  Payload = <<>>,
  Options = [],
  {ok, StatusCode, RepsHeaders, ClientRef} = hackney:request(Method, URL, Headers, Payload, Options),
  {ok, StatusCode, RepsHeaders, ClientRef}.

delete_user(Id)->
  application:ensure_all_started(hackney),
  URL = url_address()++"users/"++Id,
  {ok, _, _, _} = delete_request(URL).

% Add User Section
add_user(Name)->
  application:ensure_all_started(hackney),

  PostBody = "{	\"name\": \""++Name++"\"}",
  ReqBody = list_to_binary(PostBody),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  Path = <<"localhost:8080/users">>,
  Method = post,
  {ok, ClientRef} = hackney:request(Method, Path, ReqHeaders, stream, []),
  ok  = hackney:send_body(ClientRef, ReqBody),
  {ok, _Status, _Headers, ClientRef} = hackney:start_response(ClientRef),
  {ok, _} = hackney:body(ClientRef).