-module(batch_crash).

-behaviour(gen_batch).
-export([init/1, process_item/3, worker_died/5, job_stopping/1, job_complete/2]).

init([]) ->
    {ok, 1, [one], []}.

process_item(one, _StartTime, []) ->
    erlang:error(fubar).

worker_died(_, _WorkerPid, _StartTime, _Info, []) ->
    erlang:error(fubar).

job_stopping([]) ->
    erlang:error(fubar).

job_complete(_Status, []) ->
    erlang:error(fubar).

