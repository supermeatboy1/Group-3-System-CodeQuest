-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 20000}}.

websocket_init([DepcacheServer]) ->
    {ok, WorkerPid} = exec_worker:start(self()),
    {ok, ValidatorPid} = code_validator:start(self()),
    State =
        #{worker_pid => WorkerPid,
          validator_pid => ValidatorPid,
          source_files => [],
          session_id => [],
          exercise_id => 1,
          user_id => 1,
          session_dir => [],
          passed_test_cases => 0,
          depcache_server => DepcacheServer,
          test_cases => #{}},
    {[], State}.

parse_json_input_message(JsonParsedMap, State) ->
    case JsonParsedMap of
        #{<<"heartbeat">> := _} ->
            {ok, "heartbeat_received", State};
        #{<<"start_coding">> := ExerciseId, <<"session_id">> := SessionIdBinary} ->
            SessionId = binary_to_list(SessionIdBinary),
            io:format("Client has sent their session id: ~p~n", [SessionId]),
            SessionDirectory = string:join(["working_dir", SessionId, ""], "/"),
            filelib:ensure_dir(SessionDirectory),
            io:format("start_coding with test id: ~p ~n", [ExerciseId]),
            {ok, TestCasesRowsNested} = db:fetch_test_cases(ExerciseId),
            TestCasesMap =
                lists:map(fun({{Input, ExpectedOutput}}) ->
                             #{input =>
                                   re:split(
                                       erlang:binary_to_list(Input), "\n"),
                               expected_out => erlang:binary_to_list(ExpectedOutput)}
                          end,
                          TestCasesRowsNested),
            io:format("Test Cases Map: ~p~n", [TestCasesMap]),
            Data =
                #{<<"files">> =>
                      [#{<<"filename">> => <<"main.py">>,
                         <<"content">> => <<"print(\"Hello world.\")">>}],
                  <<"num_test_cases">> => length(TestCasesMap)},
            NewState =
                State#{session_id := SessionId,
                       session_dir := SessionDirectory,
                       test_cases := TestCasesMap,
                       exercise_id := ExerciseId},
            {ok, jsx:encode(Data), NewState};
        #{<<"file_update">> := FileUpdateList} ->
            WorkerPid = maps:get(worker_pid, State),
            {ok, WorkerRunning} = gen_server:call(WorkerPid, get_running_state),
            ValidatorPid = maps:get(validator_pid, State),
            {ok, ValidatorRunning} = gen_server:call(ValidatorPid, get_running_state),

            Combined = {WorkerRunning, ValidatorRunning},
            case Combined of
                {false, false} ->
                    OldSourceFiles = maps:get(source_files, State),
                    SessionDirectory = maps:get(session_dir, State),
                    SourceFiles =
                        file_handler:parse_file_actions(FileUpdateList,
                                                        OldSourceFiles,
                                                        SessionDirectory),
                    io:format("Current source files: ~p~n", [SourceFiles]),
                    NewState = State#{source_files := SourceFiles},
                    {ok, [], NewState};
                _ ->
                    {ok, [], State}
            end;
        #{<<"try_execute">> := _} ->
            NewState = State#{ passed_test_cases := 0 },
            WorkerPid = maps:get(worker_pid, NewState),
            SessionDirectory = maps:get(session_dir, NewState),
            SessionId = maps:get(session_id, NewState),
            {ok, WorkerRunning} = gen_server:call(WorkerPid, get_running_state),
            io:format("Is worker already running? ~p ~n", [WorkerRunning]),
            case WorkerRunning of
                false ->
                    exec_worker:execute_python(WorkerPid, SessionDirectory, SessionId),
                    {ok, jsx:encode(#{<<"exec_start">> => true}), NewState};
                _ ->
                    {ok, jsx:encode(#{<<"error">> => <<"exec_already_running">>}), State}
            end;
        #{<<"try_validate">> := _} ->
            NewState = State#{ passed_test_cases := 0 },
            io:format("Client wants to validate...~n"),
            ValidatorPid = maps:get(validator_pid, NewState),
            TestCasesMap = maps:get(test_cases, NewState),
            SessionDirectory = maps:get(session_dir, NewState),
            SessionId = maps:get(session_id, NewState),
            {ok, ValidatorRunning} = gen_server:call(ValidatorPid, get_running_state),
            io:format("Is validator already running? ~p ~n", [ValidatorRunning]),
            case ValidatorRunning of
                false ->
                    code_validator:validate_python(ValidatorPid,
                                                   TestCasesMap,
                                                   SessionDirectory,
                                                   SessionId),
                    {ok, jsx:encode(#{<<"exec_start">> => true}), NewState};
                _ ->
                    {ok, jsx:encode(#{<<"error">> => <<"validator_already_running">>}), NewState}
            end;
        #{<<"close_terminal">> := _} ->
            WorkerPid = maps:get(worker_pid, State),
            SessionId = maps:get(session_id, State),
            io:format("Request to close terminal~n"),
            exec_worker:stop_task(WorkerPid, SessionId),
            {ok, [], State};
        #{<<"terminal_input">> := Input} ->
            WorkerPid = maps:get(worker_pid, State),
            exec_worker:terminal_input(WorkerPid, Input),
            {ok, [], State};
        #{} ->
            {undefined, [], State}
    end.

websocket_handle({text, Msg}, StateOld) ->
    case jsx:is_json(Msg) of
        true ->
            JsonDecoded = jsx:decode(Msg),
            {Status, ReturnValue, State} = parse_json_input_message(JsonDecoded, StateOld),
            case Status of
                ok ->
                    {[{text, ReturnValue}], State};
                _ ->
                    {ReturnValue, State}
            end;
        _ ->
            {[], StateOld}
    end;
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({text, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info({'$gen_cast', {exec_worker_stdout, Msg}}, State) ->
    JsonReturn = jsx:encode(#{<<"console_update">> => #{<<"stdout">> => Msg}}),
    {[{text, JsonReturn}], State};
websocket_info({'$gen_cast', {exec_worker_stderr, Msg}}, State) ->
    JsonReturn = jsx:encode(#{<<"console_update">> => #{<<"stderr">> => Msg}}),
    {[{text, JsonReturn}], State};
websocket_info({'$gen_cast', {exec_worker_proc_end, Reason}}, State) ->
    case Reason of
        {exit_status, Num} ->
            Reason2 = #{<<"exit_status">> => Num};
        normal ->
            Reason2 = normal;
        _ ->
            Reason2 = undefined
    end,
    JsonReturn = jsx:encode(#{<<"console_update">> => #{<<"exit">> => Reason2}}),
    {[{text, JsonReturn}], State};
websocket_info({'$gen_cast', {code_validation_ok, TestCaseNumber}}, StateOld) ->
    TotalTestCases = length(maps:get(test_cases, StateOld)),
    PassedTestCases = maps:get(passed_test_cases, StateOld) + 1,
    NewScore = (PassedTestCases * 100) / TotalTestCases,
    JsonReturn = jsx:encode(#{
        <<"code_validation_ok">> => TestCaseNumber,
        <<"new_score">> => NewScore
    }),
    ExerciseId = maps:get(exercise_id, StateOld),
    SessionId = maps:get(session_id, StateOld),
    DepcacheServer = maps:get(depcache_server, StateOld),
    UserId = session:get_id_from_session(SessionId, DepcacheServer),
    case UserId of
        undefined -> ok;
        _ -> db:update_score(UserId, ExerciseId, NewScore)
    end,
    State = StateOld#{ passed_test_cases := PassedTestCases },
    {[{text, JsonReturn}], State};
websocket_info({'$gen_cast', {code_validation_fail, TestCaseNumber}}, State) ->
    JsonReturn = jsx:encode(#{<<"code_validation_fail">> => TestCaseNumber}),
    {[{text, JsonReturn}], State};
websocket_info({'$gen_cast', _Msg}, State) ->
    {[], State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _PartialReq, State) ->
    WorkerPid = maps:get(worker_pid, State),
    case is_process_alive(WorkerPid) of
        true -> gen_server:stop(WorkerPid);
        _ -> ok
    end,

    ValidatorPid = maps:get(validator_pid, State),
    case is_process_alive(ValidatorPid) of
        true -> gen_server:stop(ValidatorPid);
        _ -> ok
    end,

    SessionDirectory = maps:get(session_dir, State),
    case SessionDirectory of
        [] -> ok;
        _ -> directory:del_dir(SessionDirectory)
    end,
    ok.
