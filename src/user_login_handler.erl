-module(user_login_handler).

-export([init/2]).

-define(TIMEOUT, 86400). % 24 hours

init(Req, [DepcacheServer]) ->
    Cookies =
        maps:from_list(
            cowboy_req:parse_cookies(Req)),
    HasBody = cowboy_req:has_body(Req),
    Method = cowboy_req:method(Req),
    case session:get_id_from_session(binary_to_list(maps:get(<<"session_id">>,
                                                             Cookies,
                                                             <<"">>)),
                                     DepcacheServer)
    of
        undefined ->
            Reply = handle(Method, HasBody, DepcacheServer, Cookies, Req);
        _ ->
            BodyTemplate =
                cache_util:get_file_string(already_logged_in_html,
                                           "priv/templates/alreadyLoggedIn.html",
                                           DepcacheServer),
            {ok, Compiled} = erlte:compile(BodyTemplate),
            Message =
                [{message, "You are already logged in."},
                 {logged_in_btn_uri, "/dashboard.html"},
                 {logged_in_btn_text, "Dashboard"}],
            {ok, Body} = erlte:render(Compiled, Message),
            Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req)
    end,
    {ok, Reply, [DepcacheServer]}.

handle_post(Map) ->
    case maps:get(<<"register">>, Map, undefined) of
        <<"Register">> ->
            Email = string:trim(binary_to_list(maps:get(<<"email">>, Map))),
            FirstName = string:trim(binary_to_list(maps:get(<<"firstname">>, Map))),
            LastName = string:trim(binary_to_list(maps:get(<<"lastname">>, Map))),
            Password = fun() -> binary_to_list(maps:get(<<"password">>, Map)) end,
            db:register(FirstName, LastName, Email, Password);
        _ ->
            case maps:get(<<"sign_in">>, Map, undefined) of
                <<"Sign In">> ->
                    FnameOrEmail = string:trim(binary_to_list(maps:get(<<"fname_or_email">>, Map))),
                    Password = binary_to_list(maps:get(<<"password">>, Map)),
                    db:login(FnameOrEmail, Password);
                _ ->
                    invalid
            end
    end.

handle(<<"GET">>, _, DepcacheServer, Cookies, Req) ->
    Body =
        cache_util:get_file_string(login_html, "priv/templates/login.html", DepcacheServer),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
handle(<<"POST">>, true, DepcacheServer, Cookies, Req0) ->
    {ok, PostValList, Req} = cowboy_req:read_urlencoded_body(Req0),
    PostVals = maps:from_list(PostValList),

    Result = handle_post(PostVals),

    BodyTemplate =
        cache_util:get_file_string(already_logged_in_html,
                                   "priv/templates/alreadyLoggedIn.html",
                                   DepcacheServer),
    {ok, Compiled} = erlte:compile(BodyTemplate),

    case Result of
        {ok, DbMessage, SessionType, UserIdInt, FullName} ->
            UserId = integer_to_list(UserIdInt),
            SessionCookie =
                binary_to_list(binary:encode_hex(
                                   crypto:strong_rand_bytes(16))),
            session:start(UserId, SessionCookie, SessionType, FullName, DepcacheServer),
            Message =
                [{message, DbMessage},
                 {logged_in_btn_uri, "/dashboard.html"},
                 {logged_in_btn_text, "Dashboard"}],
            Req2 =
                cowboy_req:set_resp_cookie(<<"session_id">>,
                                           SessionCookie,
                                           Req,
                                           #{max_age => ?TIMEOUT});
        {error, DbMessage} ->
            Message =
                [{message, DbMessage},
                 {logged_in_btn_uri, "/login.html"},
                 {logged_in_btn_text, "Back to Login Page"}],
            Req2 = Req
    end,

    {ok, Body} = erlte:render(Compiled, Message),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req2);
handle(<<"POST">>, false, DepcacheServer, Cookies, Req) ->
    BodyTemplate =
        cache_util:get_file_string(already_logged_in_html,
                                   "priv/templates/alreadyLoggedIn.html",
                                   DepcacheServer),
    {ok, Compiled} = erlte:compile(BodyTemplate),
    Message =
        [{message, "Missing form body."},
         {logged_in_btn_uri, "/login.html"},
         {logged_in_btn_text, "Back to Login"}],
    {ok, Body} = erlte:render(Compiled, Message),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
handle(_, _, _DepcacheServer, _Cookies, Req) ->
    cowboy_req:reply(405,
                     #{<<"content-type">> => <<"text/html">>},
                     <<"Method Not Allowed">>,
                     Req).
