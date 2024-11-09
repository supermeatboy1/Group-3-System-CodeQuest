%%%-------------------------------------------------------------------
%% @doc codequest_server public API
%% @end
%%%-------------------------------------------------------------------

-module(codequest_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, DepcacheServer} = depcache:start_link([]),
    syn:add_node_to_scopes([users]),
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/code_ws", websocket_handler, [DepcacheServer]},
                                 {"/login.html", user_login_handler, [DepcacheServer]},
                                 {"/dashboard.html", dashboard_handler, [DepcacheServer]},
                                 {"/exercise/", exercise_handler, [DepcacheServer]},
                                 {"/",
                                  cowboy_static,
                                  {priv_file, codequest_server, "static/index.html"}},
                                 {"/[...]",
                                  cowboy_static,
                                  {priv_dir,
                                   codequest_server,
                                   "static/",
                                   [{mimetypes, cow_mimetypes, web}]}}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 6588}], #{env => #{dispatch => Dispatch}}),
    codequest_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
