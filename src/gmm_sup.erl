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

init([]) ->
    %% create ets tables
    settings:create_ets(),
    outbox:create_ets(),
    inbox:create_ets(),

    %% spawn child processes
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    RedisSpec = #{
        id => ?REDIS_SERVER,
        start => {persistence, create_redis_client, []}
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
