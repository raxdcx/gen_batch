%%%-------------------------------------------------------------------
%%% @author Matthew Fitzpatrick
%%% Created : 26. Mar 2014 1:01 PM
%%%-------------------------------------------------------------------
-module(batch_worker_results).
-author("matthew_fitzpatrick").

-behaviour(gen_batch).
-export([init/1, process_item/3, worker_died/5, job_stopping/1, job_complete/2]).

init([]) ->
  {ok, 2, [1, 2, 3], []}.

process_item(Item, _StartTime, []) ->
  {result, Item}.

worker_died(_, _WorkerPid, _StartTime, _Info, []) ->
  ok.

job_stopping([]) ->
  ok.

job_complete(_Status, []) ->
  ok.
