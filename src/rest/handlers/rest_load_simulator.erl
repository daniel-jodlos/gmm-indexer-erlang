%%%-------------------------------------------------------------------
%% @doc
%%  Implements API for request of load simulation
%% @end
%%%-------------------------------------------------------------------

-module(rest_load_simulator).
-behavior(cowboy_handler).

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    resource_exists/2
]).

-export([
    from_json/2
]).


%%%---------------------------
%% cowboy_rest callbacks
%%%---------------------------

%% Dialyzer keeps warning that {ok, JsonMap} cannot match {error, any()}
%% but it's intentional - if JSON is wrong, I want it to throw error
-dialyzer({nowarn_function, init/2}).
init(Req0, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    {ok, JsonMap} = parse_decoded_body(gmm_utils:decode(Data)),
    NewState = maps:merge(State, #{body => JsonMap}),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, State) ->
    io:format("~p\n\n", [maps:get(body, State)]),
    {true, Req, State}.


%%%---------------------------
%% internal functions
%%%---------------------------

-spec parse_decoded_body(any()) -> {ok, map()} | {error, any()}.
parse_decoded_body(#{<<"ops">> := List}) when is_list(List) ->
    Validator =
        fun(#{<<"t">> := Type, <<"f">> := From, <<"to">> := To, <<"p">> := Permissions, <<"tr">> := Trace})
            when Type =:= <<"a">>; Type =:= <<"r">>; Type =:= <<"p">> ->
            {ok, #{op_type => Type, from => From, to => To, permissions => Permissions, trace => Trace}};
            (_) -> {error, "Invalid JSON"}
        end,
    ValidatedList = lists:map(Validator, List),
    case lists:all(fun({ok, _}) -> true; (_) -> false end, ValidatedList) of
        true -> {ok, ValidatedList};
        false -> {error, "Parsing JSON error"}
    end;
parse_decoded_body(_) ->
    {error, "Invalid JSON"}.
