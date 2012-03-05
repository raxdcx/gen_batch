-module(gen_batch_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    supervisor:start_link({local, gen_batch_sup}, gen_batch_sup, []).

stop(_State) ->
    ok.

