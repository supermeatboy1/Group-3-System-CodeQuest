%%%-------------------------------------------------------------------
%% @doc codequest_server public API
%% @end
%%%-------------------------------------------------------------------

-module(codequest_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", codequest_handler_websocket, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 6588}], #{env => #{dispatch => Dispatch}}),
    codequest_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
