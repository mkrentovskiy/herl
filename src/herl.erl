-module(herl).
-export([start/0, stop/0, env/2]).

start() ->
    ensure_started(crypto),
    application:start(herl).

stop() ->
	application:stop(herl).

env(Param, Default) ->
        case application:get_env(?MODULE, Param) of
                {ok, Value} -> Value;
                _ -> Default
        end.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
