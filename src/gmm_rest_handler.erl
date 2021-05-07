-module(gmm_rest_handler).

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
    id_exists/1
]).

-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
    {cowboy_rest, Req, State}.

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

resource_exists(Req, State) ->
    IdRaw = cowboy_req:binding(id, Req),
    Id = case IdRaw of
             undefined -> undefined;
             Bin -> binary_to_integer(Bin)
         end,
    {id_exists(Id), Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    IdRaw = cowboy_req:binding(id, Req),
    Id = binary_to_integer(IdRaw),
    ets:delete(?USERS_TABLE, Id),
    {true, Req, State}.

delete_completed(Req, State) ->
    {false, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inner function of resource_exists callback
id_exists(undefined) ->
    true;
id_exists(error) ->
    false;
id_exists(Id) ->
    case ets:lookup(users_table, Id) of
        [] -> false;
        _ -> true
    end.

%% POST handler
handle_post(Name) ->
    Id = ets:lookup_element(?USERS_TABLE, next_id, 2),
    ets:insert(?USERS_TABLE, {Id, Name}),
    ets:update_element(?USERS_TABLE, next_id, {2, Id+1}),
    json_utils:encode(#{<<"id">> => Id}).

from_text(Req0, State) ->
    {ok, DataRaw, Req} = cowboy_req:read_body(Req0),
    case json_utils:decode(DataRaw) of
        #{<<"name">> := Name} -> {{true, handle_post(Name)}, Req, State};
        _ -> {false, Req, State}
    end.

%% GET handler
handle_get(listing) ->
    UsersList = lists:filter(
        fun(Elem) -> case Elem of
                         {next_id, _} -> false;
                         _Any -> true
                     end
        end, ets:tab2list(?USERS_TABLE)),
    {UsersIds, _} = lists:unzip(UsersList),
    json_utils:encode(#{<<"users">> => UsersIds});
handle_get(Id) ->
    case ets:lookup(?USERS_TABLE, Id) of
        [] -> json_utils:empty_json();
        [{_, Name} | _] -> json_utils:encode({[{<<"id">>, Id}, {<<"name">>, Name}]})
    end.

to_text(Req, State) ->
    IdRaw = cowboy_req:binding(id, Req),
    Id = case IdRaw of
             undefined -> listing;
             Bin -> binary_to_integer(Bin)
         end,
    {handle_get(Id), Req, State}.
