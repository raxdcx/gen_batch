-module(gen_batch).

-export([behaviour_info/1]).
-export([run_job/2, sync_run_job/2]).


behaviour_info(callbacks) -> [
    {init, 1},
    {process_item, 3},
    {worker_died, 5},
    {job_stopping, 1},
    {job_complete, 2}
].

run_job(Callback, Args) ->
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [Callback]),
    gen_batch_runner:run_job(Pid, Args).

sync_run_job(Callback, Args) ->
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [Callback]),
    gen_batch_runner:sync_run_job(Pid, Args).
