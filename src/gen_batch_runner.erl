-module(gen_batch_runner).
-behaviour(gen_fsm).

%% API
-export([start_link/1, run_job/2, sync_run_job/2, stop/1]).

%% Worker callback
-export([worker_ready/2, worker_ready/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% State callbacks
-export([ready/2, ready/3, running/2, complete/2]).

-record(state, { callback    :: module(), % module
                 num_workers :: non_neg_integer(),
                 items       :: term(), % queue
                 active      :: term(), % orddict
                 from        :: pid() | undefined,
                 reason      :: undefined | stopped | complete,
                 job_state   :: gen_batch:job_state() }). % defined by callback module


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(module()) -> {ok, pid()} | {error, term()}.
start_link(Callback) ->
    gen_fsm:start_link(?MODULE, [Callback], []).

-spec run_job(pid(), any()) -> ok.
run_job(Pid, Args) ->
    gen_fsm:send_event(Pid, {run_job, Args}).

-spec sync_run_job(pid(), any()) -> ok | {error, term()}.
sync_run_job(Pid, Args) ->
    gen_fsm:sync_send_event(Pid, {run_job, Args}, infinity).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).


%%%===================================================================
%%% Worker callback
%%%===================================================================

-spec worker_ready(pid(), pid()) -> ok.
worker_ready(Pid, WorkerPid) ->
    worker_ready(Pid, WorkerPid, ok).

-spec worker_ready(pid(), pid(), ok | stop) -> ok.
worker_ready(Pid, WorkerPid, Continue) ->
    gen_fsm:send_event(Pid, {worker_ready, WorkerPid, Continue}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Callback]) ->
    error_logger:info_msg("Job runner ~p (~p) starting up...~n", [Callback, self()]),
    {ok, ready, #state{ callback = Callback,
                        active = orddict:new() }}.

ready({run_job, Args}, S) ->
    ready({run_job, Args}, undefined, S).

ready({run_job, Args}, From, #state{ callback = Callback } = S) ->
    case Callback:init(Args) of
        {ok, NumWorkers, Items, JobState} ->
            start_workers(NumWorkers, Callback),
            {next_state, running, S#state{ from = From,
                                           num_workers = NumWorkers,
                                           items = queue:from_list(Items),
                                           job_state = JobState }};

        {stop, Reason} ->
            reply(From, {error, Reason}),
            {stop, normal, S}
    end.

running({worker_ready, WorkerPid, ok}, #state{items = Items, job_state = JobState} = S) ->
    case queue:out(Items) of
        {empty, I2} ->
            Active = stop_worker(WorkerPid, S),
            wind_down(S#state{ items = I2, active = Active, reason = complete });

        {{value, Item}, I2} ->
            StartTime = now(),
            gen_batch_worker:process(WorkerPid, Item, StartTime, JobState),
            Active = orddict:store(WorkerPid, {Item, StartTime}, S#state.active),
            {next_state, running, S#state{ items = I2, active = Active }}
    end;

running({worker_ready, WorkerPid, stop}, #state{callback = Callback, job_state = JobState} = S) ->
    Active = stop_worker(WorkerPid, S),
    spawn(Callback, job_stopping, [JobState]),
    error_logger:info_msg("Job runner ~p (~p) shutting down...~n", [Callback, self()]),
    wind_down(S#state{ items = queue:new(), active = Active, reason = stopped }).

complete({worker_ready, WorkerPid, _}, S) ->
    Active = stop_worker(WorkerPid, S),
    wind_down(S#state{ active = Active }).

handle_info({'DOWN', _, process, _Pid, normal}, StateName, S) ->
    %% This is a worker we told to stop
    %% Nothing to see here...move along
    {next_state, StateName, S};

handle_info({'DOWN', _, process, WorkerPid, shutdown}, _StateName, S) ->
    %% This worker has been shanked by its supervisor
    %% More than likely the supervisor has crashed/restarted and we
    %% cannot continue processing the job since there are no more workers
    error_logger:warning_msg("Worker ~p shutdown~n", [WorkerPid]),
    {Item, StartTime, Active} = clear_worker(WorkerPid, S),

    Callback = S#state.callback,
    spawn(Callback, worker_died, [Item, WorkerPid, StartTime, shutdown, S#state.job_state]),

    wind_down(S#state{ active = Active, reason = stopped });

handle_info({'DOWN', _, process, WorkerPid, Info}, StateName, S) ->
    error_logger:warning_msg("Worker ~p crashed: ~p~n", [WorkerPid, Info]),
    {Item, StartTime, Active} = clear_worker(WorkerPid, S),

    Callback = S#state.callback,
    spawn(Callback, worker_died, [Item, WorkerPid, StartTime, Info, S#state.job_state]),

    %% Start a replacement worker
    start_workers(1, Callback),
    {next_state, StateName, S#state{ active = Active }};

handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_event(stop, _StateName, #state{callback = Callback} = S) ->
    spawn(Callback, job_stopping, [S#state.job_state]),
    error_logger:info_msg("Job runner ~p (~p) shutting down...~n", [Callback, self()]),
    wind_down(S#state{ items = queue:new(), reason = stopped });

handle_event(_Event, _StateName, State) ->
    {stop, unsupportedOperation, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, unsupportedOperation, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_workers(NumWorkers, Callback) ->
    lists:foreach(fun(_) ->
                      {ok, Pid} = gen_batch_worker_sup:start_worker(self(), Callback),
                      error_logger:info_msg("Runner ~p starting job worker with pid ~p~n", [self(), Pid]),
                      erlang:monitor(process, Pid)
                  end, lists:seq(1, NumWorkers)).

stop_worker(WorkerPid, #state{ active = Active }) ->
    gen_batch_worker:stop(WorkerPid),
    orddict:erase(WorkerPid, Active).

clear_worker(WorkerPid, #state{ active = Active }) ->
    {Item, StartTime} = orddict:fetch(WorkerPid, Active),
    A2 = orddict:erase(WorkerPid, Active),
    {Item, StartTime, A2}.

reply(From, Reply) ->
    case From of
        undefined -> ok;
        From -> gen_fsm:reply(From, Reply)
    end.

wind_down(#state{ callback = Callback, reason = Reason } = S) ->
    case orddict:size(S#state.active) of
        0 ->
            error_logger:info_msg("Job ~p (~p) ~p.~n", [Callback, self(), Reason]),
            spawn(Callback, job_complete, [Reason, S#state.job_state]),
            reply(S#state.from, ok),
            {stop, normal, S};

        _ ->
            {next_state, complete, S}
    end.
