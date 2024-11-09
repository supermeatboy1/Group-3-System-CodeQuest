-module(exercise_handler).

-export([init/2]).
-export([message_template/2]).

message_template(Input, DepcacheServer) ->
    BodyTemplate =
        cache_util:get_file_string(already_logged_in_html,
                                   "priv/templates/alreadyLoggedIn.html",
                                   DepcacheServer),
    {ok, Compiled} = erlte:compile(BodyTemplate),
    Message =
        [{message, Input},
         {logged_in_btn_uri, "/dashboard.html"},
         {logged_in_btn_text, "Dashboard"}],
    erlte:render(Compiled, Message).

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

create_exercise_page({ok, undefined}, _, DepcacheServer) ->
    message_template("Invalid code exercise page.", DepcacheServer);
create_exercise_page({ok, Result}, ExerciseIdStr, DepcacheServer) ->
    BodyTemplate =
        cache_util:get_file_string(exercise_index_html,
                                   "priv/templates/exerciseIndex.html",
                                   DepcacheServer),
    {ok, Compiled} = erlte:compile(BodyTemplate),
    Title = maps:get("title", Result),
    Description = maps:get("description", Result),
    Message =
        [{exercise_title, Title},
         {exercise_description, Description},
         {exercise_id, ExerciseIdStr}],
    erlte:render(Compiled, Message);
create_exercise_page({error, Msg}, _, DepcacheServer) ->
    message_template("Error in fetching exercise: " ++ Msg, DepcacheServer).

handle(<<"GET">>, DepcacheServer, UserId, Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    case lists:keyfind(<<"exercise_id">>, 1, QsVals) of
        {_, ExerciseIdStr} ->
            io:format("ExerciseId: ~p~n", [ExerciseIdStr]),
            case session:get_session_type_from_id(UserId, DepcacheServer) of
                student ->
                    DbResult = db:fetch_exercise(binary_to_integer(ExerciseIdStr)),
                    {ok, Body} = create_exercise_page(DbResult, ExerciseIdStr, DepcacheServer);
                teacher ->
                    {ok, Body} =
                        message_template("Only students are allowed to take coding exercises.",
                                         DepcacheServer);
                _ ->
                    Body = <<"Invalid session type.">>
            end;
        _ ->
            {ok, Body} = message_template("Invalid code exercise page.", DepcacheServer)
    end,

    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
handle(_, _, _, Req) ->
    cowboy_req:reply(405,
                     #{<<"content-type">> => <<"text/html">>},
                     <<"Method Not Allowed">>,
                     Req).
