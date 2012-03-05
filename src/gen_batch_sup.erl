-module(gen_batch_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    JobRunnerSup = ?CHILD(gen_batch_runner_sup, supervisor),
    JobWorkerSup = ?CHILD(gen_batch_worker_sup, supervisor),
    {ok, {{one_for_one, 10, 10}, [JobRunnerSup, JobWorkerSup]}}.
