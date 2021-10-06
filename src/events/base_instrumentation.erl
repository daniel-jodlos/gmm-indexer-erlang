-module(base_instrumentation).
-author("Daniel Jodłoś").

-export([
    event_started/2,
    event_finished/2
]).

%% TODO: transform to genserver cast calls, those operations cannot be blocking. It's ok for now, but it won't stay like this for very long
%% TODO: let's extract typespecs to header file and import it here, it really will be more usefull this way

event_started(Vertex, Event) ->
    io:format("Event processing started ~p @ ~w: ~p~n", [Vertex, calendar:local_time(), Event]),
    ok.

event_finished(Vertex, Event) ->
    % io:format("Event processing finished ~p @ ~w: ~p~n", [Vertex, calendar:local_time(), Event]),
    ok.

