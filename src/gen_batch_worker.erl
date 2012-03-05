-module(gen_batch_worker).

%% API
-export([start_link/2, process/4, stop/1]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {runner, callback}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Runner, Callback) ->
    gen_server:start_link(?MODULE, {Runner, Callback}, []).

process(Pid, Item, StartTime, JobState) ->
    gen_server:cast(Pid, {process, Item, StartTime, JobState}).

stop(Pid) ->
    %% Don't raise a noproc error if the worker has already died
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, stop);
        false -> ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Runner, Callback}) ->
    gen_batch_runner:worker_ready(Runner, self()),
    {ok, #state{ runner = Runner, callback = Callback }}.

handle_call(stop, _From, State) ->
    error_logger:info_msg("Job worker ~p shutting down...~n", [self()]),
    {stop, normal, ok, State}.

handle_cast({process, Item, StartTime, JobState}, S) ->
    error_logger:info_msg("Processing item with worker ~p~n", [self()]),

    Callback = S#state.callback,
    Continue = try
                   Callback:process_item(Item, StartTime, JobState)
               catch
                   throw: Error ->
                       error_logger:error_report(Error),
                       ok
               end,

    gen_batch_runner:worker_ready(S#state.runner, self(), Continue),
    {noreply, S}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
