-module(gen_batch_runner_tests).
-include_lib("eunit/include/eunit.hrl").

job_runner_test_() ->
  {setup,
  fun() ->
    error_logger:tty(false),
    gen_batch_sup:start_link()
  end,
  fun({ok, Pid}) ->
    exit(Pid, normal)
  end,
  [
  {"Normal jobs should complete successfully",
  ?_assertEqual(ok, gen_batch:sync_run_job(batch_normal, []))},
  {"Exceptions thrown from callback modules shouldn't crash the job",
  ?_assertEqual(ok, gen_batch:sync_run_job(batch_throw, []))},
  {"Errors in callback modules shouldn't crash the job",
  ?_assertEqual(ok, gen_batch:sync_run_job(batch_crash, []))},
  {"Errors in callback module init should be returned to the caller",
  ?_assertEqual({error, testing}, gen_batch:sync_run_job(batch_init_fail, []))},
  {"The runner should stop when a worker asks",
  ?_assertEqual(ok, gen_batch:sync_run_job(batch_worker_stop, []))},
  {"The runner should stop when caller asks",
  ?_test(begin
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [batch_wait]),
    gen_batch_runner:run_job(Pid, []),
    ?assert(is_process_alive(Pid)),
    gen_batch_runner:stop(Pid),
    timer:sleep(100), % time to wind down
    ?assertNot(is_process_alive(Pid))
  end
  )},
  {"The runner should continue when worker supervisor dies",
  ?_assertEqual(ok, gen_batch:sync_run_job(batch_kill_sup, []))},
  {"The workers should be able to return results that are aggregated and replied back on synchronous calls",
  ?_test(begin
    {results, Results} = gen_batch:sync_run_job(batch_worker_results, []),
    ?_assertEqual([1, 2, 3], lists:sort(Results))
   end
  )},
  {"The worker should stop when the worker is shut down",
  ?_test(begin
    {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [batch_shutdown_sup]),
    gen_batch_runner:run_job(Pid, []),
    SupPid = whereis(gen_batch_runner_sup),
    ?assert(is_process_alive(Pid)),
    ?assert(is_process_alive(SupPid)),
    timer:sleep(100), % time to wind down
    ?assertNot(is_process_alive(Pid))
  end)}]
}.

handle_info_test_() ->
  {"ignores unknown events",
  ?_assertEqual({next_state, fake_state_name, fake_state},
    gen_batch_runner:handle_info(unknown_event, fake_state_name, fake_state))}.

handle_event_test_() ->
  {"stops when receiving unknown events",
  ?_assertEqual({stop, unsupportedOperation, fake_state},
    gen_batch_runner:handle_event(unknown_event, fake_state_name, fake_state))}.

handle_sync_event_test_() ->
  {"stops when receiving unknown events",
  ?_assertEqual({stop, unsupportedOperation, fake_state},
    gen_batch_runner:handle_sync_event(unknown_event, self(), fake_state_name, fake_state))}.

code_change_test_() ->
  {"returns ok",
  ?_assertEqual({ok, fake_state_name, fake_state},
    gen_batch_runner:code_change(ignored_version, fake_state_name, fake_state, ignored_extra))}.
