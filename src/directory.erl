-module(directory).

-export([del_dir/1]).

del_dir(Dir) ->
    lists:foreach(fun(D) -> ok = file:del_dir(D) end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
    EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {ok, FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} =
        lists:foldl(fun(F, {Fs, Ds}) ->
                       Path = filename:join(Dir, F),
                       case filelib:is_dir(Path) of
                           true ->
                               {Fs, [Path | Ds]};
                           false ->
                               {[Path | Fs], Ds}
                       end
                    end,
                    {[], []},
                    FilesInDir),
    lists:foreach(fun(F) -> ok = file:delete(F) end, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
