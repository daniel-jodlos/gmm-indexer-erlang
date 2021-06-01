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

init(Req0, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    NewState = maps:merge(State, #{body => Data}),
    {cowboy_rest, Req, NewState}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

%% POST handler
from_json(Req, State) ->
    JsonMap = case gmm_utils:decode(maps:get(body, State)) of
                  #{<<"ops">> := List} when is_list(List) ->
                      ValidatedList = lists:map(
                          fun(#{<<"t">> := Type, <<"f">> := From, <<"to">> := To, <<"p">> := Permissions,
                              <<"tr">> := Trace}) when Type =:= <<"a">>; Type =:= <<"r">>; Type =:= <<"p">> ->
                              #{op_type => Type, from => From, to => To, permissions => Permissions, trace => Trace};
                              (_) -> {error, "Json object invalid"}
                          end, List),
                      case lists:any(fun({error, _}) -> true; (_) -> false end, ValidatedList) of
                          true -> {error, "Parsing Json error"};
                          false -> ValidatedList
                      end;
                  _ -> {error, "Parsing Json error"}
              end,
    case JsonMap of
        {error, _} -> {false, Req, State};
        _ ->
            %% @todo implement logic
            io:format("~p\n\n", [JsonMap]),
            {true, Req, State}
    end.


%%%---------------------------
%% internal functions
%%%---------------------------

