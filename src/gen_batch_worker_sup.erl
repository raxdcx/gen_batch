-module(gen_batch_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Runner, Callback) ->
    supervisor:start_child(?MODULE, [Runner, Callback]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Child = {none, {gen_batch_worker, start_link, []},
             temporary, 5000, worker, [gen_batch_worker]},
    {ok, {{simple_one_for_one, 3, 1}, [Child]}}.
