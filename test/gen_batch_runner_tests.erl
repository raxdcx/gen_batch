-module(gen_batch_runner_tests).
-include_lib("eunit/include/eunit.hrl").

job_runner_test_() ->
    {setup,
     fun() -> error_logger:tty(false), gen_batch_sup:start_link() end,
     fun({ok, Pid}) -> exit(Pid, normal) end,
     [
      {"Normal jobs shoul complete successfully",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_normal, []))},

      {"Exceptions thrown from callback modules shouldn't crash the job",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_throw, []))},

      {"Errors in callback modules shouldn't crash the job",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_crash, []))},

      {"Errors in callback module init should be returned to the caller",
       ?_assertMatch({error, testing}, gen_batch:sync_run_job(batch_init_fail, []))},

      {"The runner should stop when a worker asks",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_worker_stop, []))},

      {"The runner should stop when caller asks",
       ?_test(
            begin
                {ok, Pid} = supervisor:start_child(gen_batch_runner_sup, [batch_wait]),
                gen_batch_runner:run_job(Pid, []),
                ?assert(is_process_alive(Pid)),
                gen_batch_runner:stop(Pid),
                timer:sleep(100), % time to wind down
                ?assertNot(is_process_alive(Pid))
            end
       )},

      {"The runner should continue when worker supervisor dies",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_kill_sup, []))}
     ]}.
