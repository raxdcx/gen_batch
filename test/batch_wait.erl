-module(batch_wait).

-behaviour(gen_batch).
-export([init/1, process_item/3, worker_died/5, job_stopping/1, job_complete/2]).

init([]) ->
    {ok, 1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], []}.

process_item(_, _StartTime, []) ->
    timer:sleep(100).

worker_died(_, _WorkerPid, _StartTime, _Info, []) ->
    ok.

job_stopping([]) ->
    ok.

job_complete(_Status, []) ->
    ok.
