-module(gen_batch_runner_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Child = {none, {gen_batch_runner, start_link, []},
             temporary, 5000, worker, [gen_batch_runner]},
    {ok, {{simple_one_for_one, 1, 1}, [Child]}}.
