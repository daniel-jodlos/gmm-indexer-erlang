%%%-------------------------------------------------------------------
%%% @author Piotr Świderski
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2021 16:54
%%%-------------------------------------------------------------------
-module(client).
-author("Piotr Świderski").


%% API
-export([get_id_list/0, 
          get_user_info/1, 
          delete_user/1, 
          add_user/1]).

% CONST
-define(URL, "localhost:8080/").

simple_request(Method, Url) -> request(Method, Url, [], <<>>, []).

request(Method, Url, Headers, Payload, Options )->
  {ok, _, _, _} = hackney:request(Method, Url, Headers, Payload, Options).

% Get Id List Section

get_id_list() ->
  application:ensure_all_started(hackney),
  Url = ?URL++"users",
  {ok, _, _, ClientRef} = simple_request(get, Url),
  {ok, Body} = hackney:body(ClientRef),
  list_body_elements(binary:bin_to_list(Body)).

% Get User Info Section

get_user_info(Id)->
  application:ensure_all_started(hackney),
  Url = ?URL++"users/"++Id,
  {ok, _, _, ClientRef} = simple_request(get, Url),
  {ok, Body} = hackney:body(ClientRef),
  binary:bin_to_list(Body).

list_body_elements(Body) ->
  Format = fun(Element) -> io:format("~s~n", [Element]) end,
  lists:foreach(Format, string:split(Body, ",")).


% Delete User Section

delete_user(Id)->
  application:ensure_all_started(hackney),
  Url = ?URL++"users/"++Id,
  {ok, _, _, _} = simple_request(delete, Url).

% Add User Section
add_user(Name)->
  application:ensure_all_started(hackney),

  PostBody = "{	\"name\": \""++Name++"\"}",
  ReqBody = list_to_binary(PostBody),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  Path = <<"localhost:8080/users">>,
  {ok, ClientRef} = hackney:request(post, Path, ReqHeaders, stream, []),
  ok  = hackney:send_body(ClientRef, ReqBody),
  {ok, _Status, _Headers, ClientRef} = hackney:start_response(ClientRef),
  {ok, _} = hackney:body(ClientRef).

