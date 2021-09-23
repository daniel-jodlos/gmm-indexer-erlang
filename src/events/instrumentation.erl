-module(instrumentation).
-author("Daniel Jodłoś").

-export([
    event_started/1,
    event_finished/1
]).

%% TODO: transform to genserver cast calls, those operations cannot be blocking. It's ok for now, but it won't stay like this for very long
%% TODO: let's extract typespecs to header file and import it here, it really will be more usefull this way

event_started(Event) ->
    io:format("Event processing started @ ~w: ~p~n", [calendar:local_time(), Event]),
    ok.

event_finished(Event) ->
    io:format("Event processing finished @ ~w: ~p~n", [calendar:local_time(), Event]),
    ok.

