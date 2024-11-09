-module(exec_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1, execute_python/3, stop_task/2, terminal_input/2]).

start(ParentPid) ->
    gen_server:start_link(?MODULE, [ParentPid], []).

execute_python(Pid, WorkingDir, SessionId) ->
    gen_server:cast(Pid, {execute, python, WorkingDir, SessionId}).

stop_task(Pid, SessionId) ->
    gen_server:cast(Pid, {stop_task, SessionId}).

terminal_input(Pid, Input) ->
    gen_server:cast(Pid, {terminal_input, Input}).

init([ParentPid]) ->
    State =
        #{running => false,
          parent_pid => ParentPid,
          os_pid => 0,
          session_id => []},
    {ok, State}.

terminate(_, State) ->
    Running = maps:get(running, State),
    SessionId = maps:get(session_id, State),
    case Running of
        true ->
            _ = exec:run(cmd_gen:docker_kill(SessionId), [stdout, stderr, monitor, {env, [clear]}]);
        _ -> ok
    end,
    ok.

% Process management
handle_info({stdout, _Pid, Data}, State) ->
    ParentPid = maps:get(parent_pid, State),
    gen_server:cast(ParentPid, {exec_worker_stdout, Data}),
    {noreply, State};
handle_info({stderr, _Pid, Data}, State) ->
    ParentPid = maps:get(parent_pid, State),
    gen_server:cast(ParentPid, {exec_worker_stderr, Data}),
    {noreply, State};
handle_info({'DOWN', _OsPid, process, _Pid, Reason}, StateOld) ->
    State = StateOld#{running := false},
    ParentPid = maps:get(parent_pid, State),
    gen_server:cast(ParentPid, {exec_worker_proc_end, Reason}),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

% synchronous
handle_call(get_running_state, _From, State) ->
    Running = maps:get(running, State),
    {reply, {ok, Running}, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _From, State) ->
    {stop, undefined, State}.

% asynchronous
handle_cast(terminate, State) ->
    {stop, normal, State};
handle_cast({terminal_input, Input}, State) ->
    OsPid = maps:get(os_pid, State),
    Running = maps:get(running, State),
    case Running of
        true ->
            exec:send(OsPid, <<Input/binary, 10>>);
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({execute, python, WorkingDir, SessionId}, StateOld) ->
    Running = maps:get(running, StateOld),
    case Running of
        false ->
            Sep = file_handler:get_directory_separator(),
            {ok, Cwd} = file:get_cwd(),
            AbsoluteDir = Cwd ++ Sep ++ WorkingDir,
            {ok, _ExecPid, OsPid} =
                exec:run(
                    cmd_gen:docker_python(AbsoluteDir, SessionId),
                    [stdout, stderr, stdin, monitor, {env, [clear]}]),
            State = StateOld#{running := true, os_pid := OsPid, session_id := SessionId},
            {noreply, State};
        _ ->
            {noreply, StateOld}
    end;
handle_cast({stop_task, SessionId}, StateOld) ->
    Running = maps:get(running, StateOld),
    case Running of
        true ->
            _ = exec:run(cmd_gen:docker_kill(SessionId), [stdout, stderr, monitor, {env, [clear]}]),
            State = StateOld#{ session_id := [] },
            {noreply, State};
        _ -> {noreply, StateOld}
    end;
handle_cast(_, State) ->
    {stop, undefined, State}.
