-module(cache_util).

-export([get_file_string/3]).

get_file_string(Key, Filename, DepcacheServer) ->
    case depcache:get(Key, DepcacheServer) of
        undefined ->
            {ok, ContentBinary} = file:read_file(Filename),
            Content = binary:bin_to_list(ContentBinary),
            depcache:set(Key, Content, DepcacheServer);
        {ok, Content} ->
            ok
    end,
    Content.

extract_session_cookie(Cookies) ->
    ok.
