%%%-------------------------------------------------------------------
%%% @doc
%%%  Module that takes care of logic of writing notifications to .csv
%%% @end
%%%-------------------------------------------------------------------
-module(csv_writer).
-author("pawel").

%% API
-export([
    write_lines/2
]).

-include("records.hrl").


%%%---------------------------
%% Exported functions
%%%---------------------------

-spec write_lines(list(notification()), integer()) -> ok | {error, any()}.
write_lines(Lines, Size) ->
    erlang:error({not_implemented, [Lines, Size]}).
