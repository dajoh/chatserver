-module(chatserver).
-export([start/0]).

start() ->
	ensure_started(crypto),
	ensure_started(asn1),
	ensure_started(ranch),
	ensure_started(cowlib),
	ensure_started(cowboy),
	ensure_started(chatserver).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.
