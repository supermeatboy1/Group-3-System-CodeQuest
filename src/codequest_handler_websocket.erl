-module(codequest_handler_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 20000}}.

websocket_init(_State) ->
    {ok, WorkerPid} = exec_worker:start(self()),
    {ok, ValidatorPid} = code_validator:start(self()),
    SessionId =
        binary_to_list(binary:encode_hex(
                           crypto:strong_rand_bytes(16))),
    SessionDirectory = string:join(["working_dir", SessionId, ""], "/"),
    filelib:ensure_dir(SessionDirectory),
    io:format("New session directory: ~p~n", [SessionDirectory]),
    State =
        #{worker_pid => WorkerPid,
          validator_pid => ValidatorPid,
          source_files => [],
          session_id => SessionId,
          session_dir => SessionDirectory},
    {[], State}.

parse_json_input_message(JsonParsedMap, State) ->
    case JsonParsedMap of
        #{<<"heartbeat">> := _} ->
            {ok, "heartbeat_received", State};
        #{<<"start_coding">> := TestID} ->
            io:format("start_coding with test id: ~p ~n", [TestID]),
            Files =
                #{<<"files">> =>
                      [#{<<"filename">> => <<"main.py">>,
                         <<"content">> =>
                             <<"a = int(input(\"Please input number 1: \"))\nb = int(input(\"Please input number 2: \"))\n\nsum = a + b\n\nprint(sum)">>}]},
            {ok, jsx:encode(Files), State};
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
            WorkerPid = maps:get(worker_pid, State),
            SessionDirectory = maps:get(session_dir, State),
            SessionId = maps:get(session_id, State),
            {ok, WorkerRunning} = gen_server:call(WorkerPid, get_running_state),
            io:format("Is worker already running? ~p ~n", [WorkerRunning]),
            case WorkerRunning of
                false ->
                    exec_worker:execute_python(WorkerPid, SessionDirectory, SessionId),
                    {ok, jsx:encode(#{<<"exec_start">> => true}), State};
                _ ->
                    {ok, jsx:encode(#{<<"error">> => <<"exec_already_running">>}), State}
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
websocket_info({'$gen_cast', _Msg}, State) ->
    {[], State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _PartialReq, State) ->
    WorkerPid = maps:get(worker_pid, State),
    case is_process_alive(WorkerPid) of
        true ->
            gen_server:stop(WorkerPid);
        _ ->
            ok
    end,

    SessionDirectory = maps:get(session_dir, State),
    directory:del_dir(SessionDirectory),
    ok.
