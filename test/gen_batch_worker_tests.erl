-module(gen_batch_worker_tests).

-include_lib("eunit/include/eunit.hrl").

stop_test_() ->
  {"returns ok if the process is already dead",
  ?_assertEqual(ok, gen_batch_worker:stop(c:pid(0, 42, 42)))}.

handle_info_test_() ->
  {"returns `noreply`",
  ?_assertEqual({noreply, fake_state}, gen_batch_worker:handle_info(ignored_info, fake_state))}.

code_change_test_() ->
  {"returns ok",
  ?_assertEqual({ok, fake_state}, gen_batch_worker:code_change(ignored_version, fake_state, ignored_extra))}.
