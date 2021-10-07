%%%-------------------------------------------------------------------
%%% @doc
%%%  Module to create new events - there no good place to put it in
%%% @end
%%%-------------------------------------------------------------------
-module(events).
-author("pawel").

%% API
-export([
    new/5
]).

-include("records.hrl").


%%%---------------------------
%% exported functions
%%%---------------------------

-spec new(Type :: binary(), Trace :: binary(), Sender :: binary(), OriginalSender :: binary(),
    EffectiveVertices :: list(binary())) -> event().
new(Type, Trace, Sender, OriginalSender, EffectiveVertices) ->
    #{
        <<"id">> => gmm_utils:uuid(),
        <<"type">> => Type,
        <<"trace">> => Trace,
        <<"sender">> => Sender,
        <<"originalSender">> => OriginalSender,
        <<"effectiveVertices">> => EffectiveVertices
    }.
