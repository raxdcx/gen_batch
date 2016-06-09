-module(gen_batch_app_tests).
-include_lib("eunit/include/eunit.hrl").

start_test_() ->
  {setup,
  fun() ->
    {ok, Pid} = gen_batch_app:start(ignored_type, ignored_start_args),
    Pid
  end,
  fun(Pid) ->
    exit(Pid, normal),
    timer:sleep(100),
    false = is_process_alive(Pid),
    ok
  end,
  fun(Pid) ->
    {"starts the application",
    ?_assert(is_process_alive(Pid))}
  end}.

stop_test_() ->
  {"returns ok",
  ?_assertEqual(ok, gen_batch_app:stop(ignored_state))}.
