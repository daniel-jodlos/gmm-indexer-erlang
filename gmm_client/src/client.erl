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

request(Method, Url, Headers, Payload, Options )->
  {ok, StatusCode, RepsHeaders, ClientRef} = hackney:request(Method, Url, Headers, Payload, Options),
  {ok, StatusCode, RepsHeaders, ClientRef}.

% Get Id List Section

get_id_list() ->
  application:ensure_all_started(hackney),
  Url = ?URL++"users",
  {ok, _, _, ClientRef} = request(get, Url, [], <<>>, []),
  {ok, Body} = hackney:body(ClientRef),
  list_body_elements(binary:bin_to_list(Body)).

% Get User Info Section

get_user_info(Id)->
  application:ensure_all_started(hackney),
  Url = ?URL++"users/"++Id,
  {ok, _, _, ClientRef} = request(get, Url, [], <<>>, []),
  {ok, Body} = hackney:body(ClientRef),
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

delete_user(Id)->
  application:ensure_all_started(hackney),
  Url = ?URL++"users/"++Id,
  {ok, _, _, _} = request(delete, Url, [], <<>>, []).

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

