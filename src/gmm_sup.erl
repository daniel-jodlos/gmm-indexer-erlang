%%%-------------------------------------------------------------------
%% @doc gmm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gmm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

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
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => ping,
          start => {ping_pong, start_link, [ping]}
         }
        ],
    ets:new(?USERS_TABLE, [
        ordered_set, public, named_table,
        {keypos,1}, {heir,none}, {write_concurrency,false},
        {read_concurrency,false}, {decentralized_counters,false}]),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
