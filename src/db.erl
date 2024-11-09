-module(db).

-export([register/4, login/2, fetch_exercise/1, fetch_test_cases/1, update_score/3, get_submission_data/0]).

connect() ->
    epgsql:connect(#{host => "localhost",
                     username => "codequest_user",
                     password => "for_testing",
                     database => "codequest_db",
                     timeout => 4000}).

account_exists(FullName, Email) ->
    {ok, C} = connect(),
    case epgsql:equery(C,
                       "SELECT * FROM users WHERE name IN ($1, $2) OR email IN ($1, $2);",
                       [FullName, Email])
    of
        {ok, _Columns, Rows} ->
            ok = epgsql:close(C),
            case Rows of
                [] ->
                    false;
                _ ->
                    true
            end;
        {error, Error} ->
            ok = epgsql:close(C),
            io:format("[account exists] Database Error: ~p~n", [Error]),
            {error, "Database error"}
    end.

fetch_account(FnameOrEmail) ->
    {ok, C} = connect(),
    case epgsql:equery(C,
                       "SELECT * FROM users WHERE name = $1 OR email = $1;",
                       [FnameOrEmail])
    of
        {ok, Columns, Rows} ->
            ok = epgsql:close(C),
            ColumnNames =
                lists:map(fun({_, Name, _, _, _, _, _, _, _}) -> binary_to_list(Name) end, Columns),
            case Rows of
                [] ->
                    {ok, #{}};
                _ ->
                    RowValues = hd(Rows),
                    Map = maps:from_list(
                              lists:zip(ColumnNames, tuple_to_list(RowValues))),
                    {ok, Map}
            end;
        {error, Error} ->
            ok = epgsql:close(C),
            io:format("[fetch account] Database Error: ~p~n", [Error]),
            {error, "Database error"}
    end.

get_individual_submission(UserId) ->
    {ok, C} = connect(),
    case epgsql:equery(C, "SELECT u.user_id, u.name, s.submission_id, s.date_submitted FROM users u JOIN user_exercise_submissions s ON u.user_id = s.user_id WHERE u.user_id = $1;", [UserId]) of
        {ok, Columns, Rows} -> ok;
        _ -> ok
    end. 

get_submission_data() ->
    {ok, C} = connect(),
    case epgsql:squery(C,
                       "SELECT user_id, name FROM users;") of
        {ok, Columns, Rows} ->
            ok = epgsql:close(C),
            ColumnNames =
                lists:map(fun({_, Name, _, _, _, _, _, _, _}) -> binary_to_list(Name) end, Columns),
            case Rows of
                [] ->
                    {error, "Database error"};
                _ ->
                    RowValues = hd(Rows),
                    Map = maps:from_list(
                              lists:zip(ColumnNames, tuple_to_list(RowValues))),
                    io:format("Student map: ~p~n", [Map]),
                    {ok, Map}
            end;
        {error, Error} ->
            ok = epgsql:close(C),
            io:format("[get submission data] Database Error: ~p~n", [Error]),
            {error, "Database error"}
    end.

update_score(UserIdStr, ExerciseId, ScoreFloat) ->
    Score = trunc(ScoreFloat),
    UserId = list_to_integer(UserIdStr),
    {ok, C} = connect(),
    io:format("UserID: ~p, ExerciseId: ~p, Score: ~p~n", [UserId, ExerciseId, Score]),
    case epgsql:equery(C, "SELECT * FROM user_exercise_submissions WHERE exercise_id = $1 AND user_id = $1;", [ExerciseId]) of
        {ok, _Columns, Rows} ->
            case Rows of
                [] ->
                    epgsql:equery(C,
                               "INSERT INTO user_exercise_submissions(user_id, exercise_id, score, date_submitted) VALUES ($1, $2, $3, NOW())",
                               [UserId, ExerciseId, Score]),
                    epgsql:close(C);
                _ ->
                    epgsql:equery(C,
                               "UPDATE user_exercise_submissions SET score = $3, date_submitted = NOW() WHERE user_id = $1 AND exercise_id = $2",
                               [UserId, ExerciseId, Score]),
                    epgsql:close(C)
            end;
        {error, Error} ->
            io:format("[update score] Database Error: ~p~n", [Error]),
            epgsql:close(C)
    end.

fetch_exercise(ExerciseId) ->
    {ok, C} = connect(),
    case epgsql:equery(C, "SELECT * FROM exercises WHERE exercise_id = $1;", [ExerciseId]) of
        {ok, Columns, Rows} ->
            ok = epgsql:close(C),
            ColumnNames =
                lists:map(fun({_, Name, _, _, _, _, _, _, _}) -> binary_to_list(Name) end, Columns),
            case Rows of
                [] ->
                    {ok, undefined};
                _ ->
                    RowValues = hd(Rows),
                    Map = maps:from_list(
                              lists:zip(ColumnNames, tuple_to_list(RowValues))),
                    {ok, Map}
            end;
        {error, Error} ->
            ok = epgsql:close(C),
            io:format("[exercise] Database Error: ~p~n", [Error]),
            {error, "Database error"}
    end.

fetch_test_cases(ExerciseId) ->
    {ok, C} = connect(),
    case epgsql:equery(C,
                       "SELECT (input_data, expected_output) FROM exercise_test_cases WHERE exercise_id = $1 ORDER BY test_case_id;",
                       [ExerciseId])
    of
        {ok, Columns, Rows} ->
            ok = epgsql:close(C),
            ColumnNames =
                lists:map(fun({_, Name, _, _, _, _, _, _, _}) -> binary_to_list(Name) end, Columns),
            case Rows of
                [] ->
                    {ok, undefined};
                _ ->
                    %RowValues = hd(Rows),
                    %Map = maps:from_list(
                    %          lists:zip(ColumnNames, tuple_to_list(RowValues))),
                    {ok, Rows}
            end;
        {error, Error} ->
            ok = epgsql:close(C),
            io:format("[fetch test cases] Database Error: ~p~n", [Error]),
            {error, "Database error"}
    end.

register(_, _, "", _) ->
    {error, "Registration error: Missing email"};
register(_, "", _, _) ->
    {error, "Registration error: Missing last name"};
register("", _, _, _) ->
    {error, "Registration error: Missing first name"};
register(FirstName, LastName, Email, Password) ->
    {ok, C} = connect(),
    FullName = FirstName ++ " " ++ LastName,
    Salt =
        binary_to_list(binary:encode_hex(
                           crypto:strong_rand_bytes(32))),
    Combined = fun() -> Password() ++ Salt end,
    PasswordHash =
        fun() ->
           binary:encode_hex(
               crypto:hash(sha3_256, Combined()))
        end,
    case account_exists(FullName, Email) of
        true ->
            {error, "Account already exists!"};
        _ ->
            case epgsql:equery(C,
                               "INSERT INTO users(name, email, password_hash, creation, password_salt) VALUES ($1, $2, $3, NOW(), $4)",
                               [FullName, Email, PasswordHash(), Salt])
            of
                {ok, _} ->
                    ok = epgsql:close(C),
                    {ok,
                     "You are now registered.",
                     student,
                     maps:get("user_id", fetch_account(Email)),
                     maps:get("name", FullName)};
                {error, Error} ->
                    ok = epgsql:close(C),
                    io:format("[register] Database Error: ~p~n", [Error]),
                    {error, "Database error"}
            end
    end.

login("", _) ->
    {error, "Login error: Missing full name or email."};
login(_, "") ->
    {error, "Login error: Missing password."};
login(FnameOrEmail, Password) ->
    Result = fetch_account(FnameOrEmail),
    case Result of
        {ok, Map} ->
            Combined = fun() -> Password ++ maps:get("password_salt", Map) end,
            DbPasswordHash = maps:get("password_hash", Map),
            TestPasswordHash =
                binary:encode_hex(
                    crypto:hash(sha3_256, Combined())),
            case DbPasswordHash of
                TestPasswordHash ->
                    {ok,
                     "You are now logged in.",
                     case maps:get("is_teacher", Map) of
                         true ->
                             teacher;
                         _ ->
                             student
                     end,
                     maps:get("user_id", Map),
                     maps:get("name", Map)};
                _ ->
                    {error, "Invalid password."}
            end;
        _ ->
            {error, "Unexpected error while logging in."}
    end.
