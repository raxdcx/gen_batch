-module(batch_kill_sup).

-behaviour(gen_batch).
-export([init/1, process_item/3, worker_died/5, job_stopping/1, job_complete/2]).

init([]) ->
    {ok, 2, [1, 2], []}.

process_item(1, _StartTime, []) ->
    exit(whereis(gen_batch_worker_sup), stop),
    ok;
process_item(2, _, []) ->
    timer:sleep(100),
    ok.

worker_died(_, _WorkerPid, _StartTime, _Info, []) ->
    ok.

job_stopping([]) ->
    ok.

job_complete(_Status, []) ->
    ok.
