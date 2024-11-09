-module(dashboard_handler).

-export([init/2]).

init(Req, [DepcacheServer]) ->
    Cookies =
        maps:from_list(
            cowboy_req:parse_cookies(Req)),
    Method = cowboy_req:method(Req),
    case session:get_id_from_session(binary_to_list(maps:get(<<"session_id">>,
                                                             Cookies,
                                                             <<"">>)),
                                     DepcacheServer)
    of
        undefined ->
            BodyTemplate =
                cache_util:get_file_string(already_logged_in_html,
                                           "priv/templates/alreadyLoggedIn.html",
                                           DepcacheServer),
            {ok, Compiled} = erlte:compile(BodyTemplate),
            Message =
                [{message, "Please login first."},
                 {logged_in_btn_uri, "/login.html"},
                 {logged_in_btn_text, "Login"}],
            {ok, Body} = erlte:render(Compiled, Message),
            Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
        UserId ->
            io:format("UserId: ~p~n", [UserId]),
            Reply = handle(Method, DepcacheServer, UserId, Req)
    end,
    {ok, Reply, [DepcacheServer]}.

handle(<<"GET">>, DepcacheServer, UserId, Req) ->
    case session:get_session_type_from_id(UserId, DepcacheServer) of
        student ->
            BodyTemplate =
                cache_util:get_file_string(student_view_html,
                                           "priv/templates/studentView.html",
                                           DepcacheServer),
            {ok, Compiled} = erlte:compile(BodyTemplate),
            Message = [{student_name, session:get_full_name_from_id(UserId, DepcacheServer)}],
            {ok, Body} = erlte:render(Compiled, Message);
        teacher ->
            Body =
                cache_util:get_file_string(teachers_grades_html,
                                           "priv/templates/teachersGrades.html",
                                           DepcacheServer),
            Data = db:get_submission_data();
        _ ->
            Body = <<"Invalid session type.">>
    end,
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
handle(_, _, _, Req) ->
    cowboy_req:reply(405,
                     #{<<"content-type">> => <<"text/html">>},
                     <<"Method Not Allowed">>,
                     Req).
