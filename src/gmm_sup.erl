%%%-------------------------------------------------------------------
%% @doc
%%  gmm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gmm_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

create_redis_spec(Acc, N)->
    case N of
        0 -> Acc;
        _ ->
            RedisSpec = #{
                id => ?REDIS_SERVER,
                start => {persistence, create_redis_client, [N]}
            },
            create_redis_spec([RedisSpec | Acc], N - 1)
    end.

init([]) ->
    %% create ets tables
    settings:create_ets(),
    outbox:create_ets(),
    inbox:create_ets(),
    persistence:prepare_client_queue(),
    %% spawn child processes
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    %RedisSpecs = create_redis_spec([], ?CLIENT_NUMBER) ++ outbox:specs_for_supervisor(),
    RedisSpec = #{
        id => ?REDIS_SERVER,
        start => {persistence, create_redis_client, [1]}
    },
    InboxDispatcherSpec = #{
        id => << "inbox_dispatcher" >>,
        start => {inbox, start_link, []}
    },
    InstrumentationSpecs = #{
        id => <<"instrumentation">>,
        start => {instrumentation, start_link, []}
    },
    ChildSpecs = [InstrumentationSpecs, InboxDispatcherSpec, RedisSpec | outbox:specs_for_supervisor()],
    {ok, {SupFlags, ChildSpecs}}.
