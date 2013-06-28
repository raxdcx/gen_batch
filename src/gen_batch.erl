-module(gen_batch).

-export([run_job/2, sync_run_job/2]).

-ifndef(no_callbacks).

-type item()      :: term().
-type items()     :: [item()].
-type job_state() :: term().

-export_types([item/0, items/0, job_state/0]).

-callback init(Args :: term()) ->
    {ok, NumWorkers :: non_neg_integer(), Items :: items(), JobState :: job_state()} |
    {stop, Reason :: term()}.

-callback process_item(Item :: item(),
                       StartTime :: erlang:timestamp(),
                       JobState :: job_state()) -> ok | stop.

-callback worker_died(Item :: item(),
                      WorkerPid :: pid(),
                      StartTime :: erlang:timestamp(),
                      Info :: term(),
                      JobState :: job_state()) -> no_return().

-callback job_stopping(JobState :: job_state()) -> no_return().

-callback job_complete(Reason :: stopped | complete,
                       JobState :: job_state()) -> no_return().

-else.

-export([behaviour_info/1]).
behaviour_info(callbacks) -> [
    {init, 1},
    {process_item, 3},
    {worker_died, 5},
    {job_stopping, 1},
    {job_complete, 2}
].

-endif.

-spec run_job(module(), any()) -> ok.
run_job(Callback, Args) ->
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [Callback]),
    gen_batch_runner:run_job(Pid, Args).

-spec sync_run_job(module(), any()) -> ok | {error, term()}.
sync_run_job(Callback, Args) ->
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [Callback]),
    gen_batch_runner:sync_run_job(Pid, Args).
