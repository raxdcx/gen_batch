-module(batch_throw).

-behaviour(gen_batch).
-export([init/1, process_item/3, worker_died/5, job_stopping/1, job_complete/2]).

init([]) ->
    {ok, 1, [one], []}.

process_item(one, _StartTime, []) ->
    throw(fubar).

worker_died(_, _WorkerPid, _StartTime, _Info, []) ->
    throw(fubar).

job_stopping([]) ->
    throw(fubar).

job_complete(_Status, []) ->
    throw(fubar).

