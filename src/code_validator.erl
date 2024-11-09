-module(code_validator).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1, validate_python/4]).

start(ParentPid) ->
    gen_server:start_link(?MODULE, [ParentPid], []).

validate_python(Pid, TestCasesMap, WorkingDir, SessionId) ->
    gen_server:cast(Pid, {start_testing, python, TestCasesMap, WorkingDir, SessionId}).

init([ParentPid]) ->
    State =
        #{running => false,
          parent_pid => ParentPid,
          os_pid => 0,
          test_cases_tail => [],
          output => [],
          current_expected_output => [],
          test_case_number => 0,
          working_dir => [],
          session_id => []},
    {ok, State}.

terminate(_, _State) ->
    ok.

% Process management
handle_info({stdout, _Pid, Data}, State) ->
    OldOutput = maps:get(output, State),
    Output = OldOutput ++ binary_to_list(Data),
    io:format("Updated output: ~p~n", [Output]),
    NewState = State#{output := Output},
    {noreply, NewState};
handle_info({stderr, _Pid, _Data}, State) ->
    {noreply, State};
handle_info({'DOWN', _OsPid, process, _Pid, _Reason}, StateOld) ->
    TestCaseNumber = maps:get(test_case_number, StateOld),
    CurrentExpectedOutput = maps:get(current_expected_output, StateOld),
    Output = maps:get(output, StateOld),
    io:format("Test Case #~p~nComparing:~nEXP: ~p~nOUT: ~p~n", [TestCaseNumber, CurrentExpectedOutput, Output]),
    case string:equal(CurrentExpectedOutput, Output) of
        true ->
            io:format("Test case is ok.~n"),
            ParentPid = maps:get(parent_pid, StateOld),
            gen_server:cast(ParentPid, {code_validation_ok, TestCaseNumber});
        _ ->
            io:format("Test case failed.~n"),
            ParentPid = maps:get(parent_pid, StateOld),
            gen_server:cast(ParentPid, {code_validation_fail, TestCaseNumber})
    end,
    State = StateOld#{running := false, output := []},
    TCMTail = maps:get(test_cases_tail, StateOld),
    erlang:send_after(100, self(), {test_case, TCMTail}),
    {noreply, State};
handle_info({test_case, [TCMHead | TCMTail]}, StateOld) ->
    WorkingDir = maps:get(working_dir, StateOld),
    SessionId = maps:get(session_id, StateOld),
    gen_server:cast(self(), {execute, python, WorkingDir, SessionId}),
    TestCaseNumber = maps:get(test_case_number, StateOld) + 1,
    State = StateOld#{test_cases_tail := TCMTail, test_case_number := TestCaseNumber, current_expected_output := maps:get(expected_out, TCMHead)},
    erlang:send_after(100, self(), {sample_input, TestCaseNumber, maps:get(input, TCMHead)}),
    {noreply, State};
handle_info({test_case, []}, StateOld) ->
    {noreply, StateOld};
handle_info({sample_input, TestCaseNumber, [InputHead | InputTail]}, StateOld) ->
    case maps:get(test_case_number, StateOld) of
        TestCaseNumber ->
            OsPid = maps:get(os_pid, StateOld),
            InputWithNewline = <<InputHead/binary, 10>>,
            exec:send(OsPid, InputWithNewline),

            erlang:send_after(100, self(), {sample_input, TestCaseNumber, InputTail}),
            {noreply, StateOld};
        _ -> {noreply, StateOld}
    end;
handle_info({sample_input, _TestCaseNumber, []}, StateOld) ->
    {noreply, StateOld};
handle_info(_, State) ->
    {noreply, State}.

% synchronous
handle_call(get_running_state, _From, State) ->
    Running = maps:get(running, State),
    {reply, {ok, Running}, State};
handle_call(_, _From, State) ->
    {stop, undefined, State}.

% asynchronous
handle_cast({start_testing, python, TestCasesMap, WorkingDir, SessionId}, StateOld) ->
    erlang:send_after(1000, self(), {test_case, TestCasesMap}),
    State = StateOld#{output := [], test_case_number := 0, working_dir := WorkingDir, session_id := SessionId},
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
            State = StateOld#{running := true, os_pid := OsPid},
            {noreply, State};
        _ ->
            {noreply, StateOld}
    end;
handle_cast(terminate, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {stop, undefined, State}.
