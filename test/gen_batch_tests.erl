-module(gen_batch_tests).

-include_lib("eunit/include/eunit.hrl").

run_job_test_() ->
  {inorder,
    {foreach,
    local,
    fun() ->
      error_logger:tty(false),
      gen_batch_sup:start_link()
    end,
    fun({ok, Pid}) ->
      true = exit(Pid, normal),
      timer:sleep(100)
    end,
    [{"returns results from a sync job",
    ?_test(begin
      {results, Results} = gen_batch:sync_run_job(batch_worker_results, []),
      Sorted = lists:sort(Results),
      Expected = [1, 2, 3],
      ?assertEqual(Expected, Sorted)
    end)},
    {"returns `ok` immediately from an async job",
    ?_test(begin
      Result = gen_batch:run_job(batch_worker_results, []),
      ?assertEqual(ok, Result)
    end)}]}
  }.
