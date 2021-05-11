-module(gmm_rest_handler).

-behavior(cowboy_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

-export([
    from_text/2,
    to_text/2
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
    Id = cowboy_req:binding(id, Req),
    {id_exists(Id), Req, State}.

%% DELETE callback
delete_resource(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    persistence:delete_user(?REDIS_SERVER, Id),
    {true, Req, State}.

delete_completed(Req, State) ->
    {false, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inner function of resource_exists callback
id_exists(undefined) ->
    true;
id_exists(Id) ->
    case persistence:get_user(?REDIS_SERVER, Id) of
        {ok, undefined} -> false;
        {ok, _Result} -> true
    end.

%% POST handler
from_text(Req0, State) ->
    {ok, DataRaw, Req} = cowboy_req:read_body(Req0),
    case json_utils:decode(DataRaw) of
        #{<<"name">> := Name} -> {{true, handle_post(Name)}, Req, State};
        _ -> {false, Req, State}
    end.

handle_post(Name) ->
    {ok, Id} = persistence:add_user(?REDIS_SERVER, Name),
    json_utils:encode(#{<<"id">> => Id}).

%% GET handler
to_text(Req, State) ->
    Id = case cowboy_req:binding(id, Req) of
             undefined -> listing;
             Bin -> Bin
         end,
    {handle_get(Id), Req, State}.

handle_get(listing) ->
    {ok, UsersIds} = persistence:get_users(?REDIS_SERVER),
    json_utils:encode(#{<<"users">> => UsersIds});
handle_get(Id) ->
    case persistence:get_user(?REDIS_SERVER, Id) of
        {ok, undefined} -> json_utils:empty_json();
        {ok, Result} -> Result
    end.
