-module(code_validator).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1]).

start(ParentPid) ->
    gen_server:start_link(?MODULE, [ParentPid], []).
validate(Pid, TestCases, WorkingDir, SessionId) ->
    gen_server:cast(Pid, {start_testing, python, TestCases, WorkingDir, SessionId}).

init([ParentPid]) ->
    State =
        #{running => false,
          parent_pid => ParentPid,
          os_pid => 0,
          test_cases => []},
    {ok, State}.

terminate(_, _State) ->
    ok.

% Process management
handle_info({stdout, _Pid, Data}, State) ->
    %ParentPid = maps:get(parent_pid, State),
    %gen_server:cast(ParentPid, {exec_worker_stdout, Data}),
    {noreply, State};
handle_info({stderr, _Pid, Data}, State) ->
    %ParentPid = maps:get(parent_pid, State),
    %gen_server:cast(ParentPid, {exec_worker_stderr, Data}),
    {noreply, State};
handle_info({'DOWN', _OsPid, process, _Pid, Reason}, StateOld) ->
    State = StateOld#{running := false},
    %ParentPid = maps:get(parent_pid, State),
    %gen_server:cast(ParentPid, {exec_worker_proc_end, Reason}),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

% synchronous
handle_call(get_running_state, _From, State) ->
    Running = maps:get(running, State),
    {reply, {ok, Running}, State};
handle_call(_, _From, State) ->
    {stop, undefined, State}.

% asynchronous
handle_cast({start_testing, python, TestCases, WorkingDir, SessionId}, StateOld) ->
    {noreply, StateOld};
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
            State = StateOld#{running := true, os_pid := OsPid},
            {noreply, State};
        _ ->
            {noreply, StateOld}
    end;
handle_cast(terminate, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {stop, undefined, State}.
