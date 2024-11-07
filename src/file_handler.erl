-module(file_handler).

-export([parse_file_actions/3, get_directory_separator/0]).

get_directory_separator() ->
    case os:type() of
        {win32, _} ->
            "\\";
        {unix, _} ->
            "/"
    end.

apply_updates([#{<<"append">> := AppendListBinary} | TailOps],
              OldCurrentLines,
              OriginalLines) ->
    AppendList = [binary_to_list(Bin) || Bin <- AppendListBinary],
    CurrentLines = OldCurrentLines ++ AppendList,
    apply_updates(TailOps, CurrentLines, OriginalLines);
apply_updates([#{<<"equal">> := #{<<"end">> := End, <<"start">> := Start}} | TailOps],
              OldCurrentLines,
              OriginalLines) ->
    Len = End - Start,
    OldExt =
        lists:sublist(
            lists:nthtail(Start, OriginalLines), Len),
    CurrentLines = OldCurrentLines ++ OldExt,
    apply_updates(TailOps, CurrentLines, OriginalLines);
apply_updates([_ | TailOps], CurrentLines, OriginalLines) ->
    apply_updates(TailOps, CurrentLines, OriginalLines);
apply_updates([], CurrentLines, _OriginalLines) ->
    CurrentLines.

parse_file_actions([H | T], OldSourceFiles, SessionDirectory) ->
    case H of
        #{<<"action">> := Action, <<"filename">> := Filename} ->
            %io:format("Filename: ~p~nAction: ~p~n", [Filename, Action]),
            case Action of
                #{<<"update">> := UpdateOperations} ->
                    io:format("File [~p] is updated! UpdateOperations: ~p~n",
                              [Filename, UpdateOperations]),
                    FullFilename = filename:join(SessionDirectory, Filename),
                    {ok, OriginalBinary} = file:read_file(FullFilename),

                    % Use re:split instead of string:tokens to include
                    % empty lines that only contain a newline.
                    OriginalLines =
                        re:split(
                            erlang:binary_to_list(OriginalBinary), "\n"),

                    UpdatedLines = apply_updates(UpdateOperations, [], OriginalLines),
                    Flattened =
                        lists:flatten(
                            lists:join("\n", UpdatedLines)),

                    file:write_file(FullFilename, Flattened),

                    SourceFiles = OldSourceFiles,
                    NewFileCreated = false;
                #{<<"create">> := #{<<"data">> := Data}} ->
                    io:format("File [~p] is created!~n", [Filename]),
                    file:write_file(
                        filename:join(SessionDirectory, Filename), Data),

                    SourceFiles = [OldSourceFiles ++ Filename],
                    NewFileCreated = true;
                <<"delete">> ->
                    io:format("File [~p] is deleted ...~n", [Filename]),
                    SourceFiles = lists:delete([Filename], OldSourceFiles),
                    NewFileCreated = false;
                _ ->
                    SourceFiles = OldSourceFiles,
                    NewFileCreated = false
            end,
            case NewFileCreated of
                true ->
                    SourceFiles ++ parse_file_actions(T, OldSourceFiles, SessionDirectory);
                _ ->
                    ok
            end;
        _ ->
            OldSourceFiles
    end;
parse_file_actions([], SourceFiles, _SessionDirectory) ->
    SourceFiles.
