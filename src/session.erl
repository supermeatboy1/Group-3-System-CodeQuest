-module(session).

-behaviour(gen_server).

-export([init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2]).
-export([start/5, quit_user_session/1, get_id_from_session/2, get_session_type_from_id/2,
         get_full_name_from_id/2]).

-define(TIMEOUT, 86400000). % 24 hours

start(UserId, SessionCookie, SessionType, FullName, DepcacheServer) ->
    quit_user_session(UserId),
    depcache:set("SessionCookie_" ++ SessionCookie, UserId, ?TIMEOUT, DepcacheServer),
    depcache:set("SessionType_" ++ UserId, SessionType, ?TIMEOUT, DepcacheServer),
    depcache:set("UserSession_" ++ UserId, SessionCookie, ?TIMEOUT, DepcacheServer),
    depcache:set("UserFullName_" ++ UserId, FullName, ?TIMEOUT, DepcacheServer),
    gen_server:start_link(?MODULE, [UserId, SessionCookie, SessionType, DepcacheServer], []).

get_id_from_session(undefined, _) ->
    undefined;
get_id_from_session(SessionCookie, DepcacheServer) ->
    case depcache:get("SessionCookie_" ++ SessionCookie, DepcacheServer) of
        {ok, UserId} ->
            UserId;
        _ ->
            undefined
    end.

get_session_type_from_id(undefined, _) ->
    undefined;
get_session_type_from_id(UserId, DepcacheServer) ->
    case depcache:get("SessionType_" ++ UserId, DepcacheServer) of
        {ok, SessionType} ->
            SessionType;
        _ ->
            undefined
    end.

get_full_name_from_id(undefined, _) ->
    undefined;
get_full_name_from_id(UserId, DepcacheServer) ->
    case depcache:get("UserFullName_" ++ UserId, DepcacheServer) of
        {ok, FullName} ->
            FullName;
        _ ->
            undefined
    end.

quit_user_session(UserId) ->
    case syn:lookup(users, UserId) of
        {Pid, _} ->
            gen_server:stop(Pid);
        _ ->
            ok
    end.

init([UserId, SessionCookie, SessionType, DepcacheServer]) ->
    State =
        #{user_id => UserId,
          session_cookie => SessionCookie,
          session_type => SessionType,
          depcache_server => DepcacheServer},
    syn:register(users, UserId, self()),
    io:format("New user session: ~p - ~p ~n", [UserId, SessionCookie]),
    {ok, State, ?TIMEOUT}.

terminate(_, State) ->
    UserId = maps:get(user_id, State),
    SessionCookie = maps:get(session_cookie, State),
    DepcacheServer = maps:get(depcache_server, State),
    syn:unregister(users, UserId),
    depcache:flush("SessionCookie_" ++ SessionCookie, DepcacheServer),
    depcache:flush("SessionType_" ++ UserId, DepcacheServer),
    depcache:flush("UserSession_" ++ UserId, DepcacheServer),
    depcache:flush("WebSocketPid_" ++ UserId, DepcacheServer),
    depcache:flush("UserFullName_" ++ UserId, DepcacheServer),
    io:format("Terminating user: ~p - ~p ~n", [UserId, SessionCookie]),
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?TIMEOUT}.

%handle_call(_, _From, State) ->
%    {stop, undefined, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

handle_cast(terminate, State) ->
    {stop, normal, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State, ?TIMEOUT}.%handle_cast(_, State) ->
                               %    {stop, undefined, State}.
