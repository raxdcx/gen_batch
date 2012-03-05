-module(gen_batch_runner_tests).
-include_lib("eunit/include/eunit.hrl").

job_runner_test_() ->
    {setup,
     fun() -> error_logger:tty(false), gen_batch_sup:start_link() end,
     fun({ok, Pid}) -> exit(Pid, normal) end,
     [
      {"Exceptions thrown from callback modules shouldn't crash the job",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_throw, []))},

      {"Errors in callback modules shouldn't crash the job",
       ?_assertMatch(ok, gen_batch:sync_run_job(batch_crash, []))}
     ]}.
