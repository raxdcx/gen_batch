-module(gen_batch_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/2, active_workers/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Runner, Callback) ->
    supervisor:start_child(?MODULE, [Runner, Callback]).

active_workers() ->
    Props = supervisor:count_children(?MODULE),
    proplists:get_value(active, Props).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Child = {none, {gen_batch_worker, start_link, []},
             temporary, 5000, worker, [gen_batch_worker]},
    {ok, {{simple_one_for_one, 3, 1}, [Child]}}.
