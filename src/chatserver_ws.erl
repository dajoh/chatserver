-module(chatserver_ws).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% Called on HTTP request, upgrade to websocket protocol.
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% Called on new websocket client.
websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, new_client}.

%% Called when we get a text message.
websocket_handle({text, Json}, Req, State) ->
	handle_json_msg(Json),
	case State of
		new_client ->
			{reply, {text, build_client_list()}, Req, old_client};
		old_client ->
			{ok, Req, old_client}
	end;

%% Called when we get an unknown message.
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

%% Called on broadcasts from other users.
websocket_info({message, Name, Msg}, Req, State) ->
	Json = jiffy:encode({[
		{type, message},
		{from, Name},
		{text, Msg}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on connections from other users.
websocket_info({connected, Name}, Req, State) ->
	Json = jiffy:encode({[
		{type, connected},
		{user, Name}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on disconnects from other users.
websocket_info({disconnected, Name}, Req, State) ->
	Json = jiffy:encode({[
		{type, disconnected},
		{user, Name}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on unknown system messages.
websocket_info(_Info, Req, State) ->
	io:format("~p ~n", [_Info]),
	{ok, Req, State}.

%% Called when clients disconnect.
websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% Internal JSON message processor.
handle_json_msg(Json) ->
	try jiffy:decode(Json) of
		{[{<<"type">>, <<"set_name">>}, {<<"name">>, Name}]} ->
			chatserver_room:change_name(Name);
		{[{<<"type">>, <<"send_msg">>}, {<<"text">>, Text}]} ->
			chatserver_room:broadcast_message(Text)
	catch
		_ -> fuck_the_police
	end.

%% Client list builder.
build_client_list() ->
	{_, Names} = lists:unzip(chatserver_room:get_client_list()),
	jiffy:encode({[
		{type, client_list},
		{list, Names}
	]}).
