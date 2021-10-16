%%%-------------------------------------------------------------------
%%% @doc
%%%  Module for logging
%%% @end
%%%-------------------------------------------------------------------
-module(gmm_logger).
-author("pawel").

%% API
-export([
    log_error/3,
    log_cowboy_req/2
]).


%%%---------------------------
%% Exported functions
%%%---------------------------

log_error(Class, Pattern, Stacktrace) ->
    io:format("Class: ~p;\nPattern: ~p;\nStacktrace: ~p\n\n", [Class, Pattern, Stacktrace]).

log_cowboy_req(Req, State) ->
    io:format("==============================\n\n"),
    io:format("Req = ~p\n\n", [Req]),
    io:format("------------------------------\n\n"),
    io:format("State = ~p\n\n", [State]),
    io:format("==============================\n\n").
