-module(gmm_handler).

-behavior(cowboy_handler).

-export([
  init/2,
  resource_exists/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  delete_completed/2,
  delete_resource/2
]).

-export([
  to_text/2,
  from_text/2,
  lookup_id/3
]).

-record(state, {table = users_table}).

%%
%% init/2
%%
init(Req, State) ->
  {cowboy_rest, Req, State}.

% converts binary string to number
bin_to_num(Bin) ->
  String = binary_to_list(Bin),
  case string:to_integer(String) of
    {error,_} -> error;
    {N, _Rest} -> N
  end.

% empty jiffy-json
empty_json() ->
  jiffy:encode({[]}).

% creates answer for GET curl
get_method_reply(listing, Table) ->
  UsersList = lists:filter(
    fun(Elem) -> case Elem of
                   {next_id, _} -> false;
                   _Any -> true
                 end
    end, ets:tab2list(Table)),
  List = lists:map(fun({Id, _}) -> Id end, UsersList),
  jiffy:encode({[{<<"users">>, List}]});

get_method_reply(Id, Table) ->
  case ets:lookup(Table, Id) of
    [] -> empty_json();
    [{_, Name} | _] -> jiffy:encode({[{<<"id">>, Id}, {<<"name">>, binary:list_to_bin(Name)}]})
  end.

% reacts to POST curl
post_method_reaction("name", Name, State) ->
  Id = ets:lookup_element(State#state.table, next_id, 2),
  ets:insert(State#state.table, {Id, Name}),
  ets:update_element(State#state.table, next_id, {2, Id+1}),
  jiffy:encode({[{<<"id">>, Id}]});
post_method_reaction(_, _, _) ->
  empty_json().

%%
%% cowboy_rest callbacks
%%

allowed_methods(Req, State) ->
  Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_text}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, from_text}
  ], Req, State}.

% inner function of resource_exists callback
lookup_id(undefined, Req, State) ->
  {true, Req, State};
lookup_id(error, Req, State) ->
  {false, Req, State};
lookup_id(Id, Req, State) ->
  case ets:lookup(State#state.table, Id) of
    [] -> {false, Req, State};
    _ -> {true, Req, State}
  end.

resource_exists(Req, State) ->
  IdRaw = cowboy_req:binding(id, Req),
  Id = case IdRaw of
         undefined -> undefined;
         Bin -> bin_to_num(Bin)
       end,
  lookup_id(Id, Req, State).

%% POST
from_text(Req0, State) ->
  {ok, DataRaw, Req} = cowboy_req:read_body(Req0),
  {[{FieldBin, NameBin} | _]} = jiffy:decode(DataRaw),
  Field = binary:bin_to_list(FieldBin),
  Name = binary:bin_to_list(NameBin),
  Body = post_method_reaction(Field, Name, State),
  {{true, Body}, Req, State}.

%% GET
to_text(Req, State) ->
  IdRaw = cowboy_req:binding(id, Req),
  Id = case IdRaw of
         undefined -> listing;
         Bin -> bin_to_num(Bin)
       end,
  {get_method_reply(Id, State#state.table), Req, State}.

delete_completed(Req, State) ->
  {false, Req, State}.

%% DELETE
delete_resource(Req, State) ->
  IdRaw = cowboy_req:binding(id, Req),
  Id = bin_to_num(IdRaw),
  ets:delete(State#state.table, Id),
  {true, Req, State}.
