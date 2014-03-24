-module(chatserver_ws).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% ------------------------------------------------------------------
%% Websocket Callback Functions
%% ------------------------------------------------------------------

%% Called on HTTP request, upgrade to websocket protocol.
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% Called on new websocket client.
websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, []}.

%% Called when we get a JSON message.
websocket_handle({text, Json}, Req, State) ->
	try handle_ejson_message(jiffy:decode(Json), Req, State) of
		Result -> Result
	catch
		Exception ->
			io:format("JSON error: ~p~n", [Exception]),
			{ok, Req, State}
	end;

%% Called when we get an unknown message.
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

%% Called on room messages.
websocket_info(Msg, Req, State) ->
	{reply, {text, jiffy:encode(get_room_message_ejson(Msg))}, Req, State}.

%% Called when clients disconnect.
websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% ------------------------------------------------------------------
%% JSON Related Functions
%% ------------------------------------------------------------------

%% Handles join messages.
handle_ejson_message({[{<<"type">>, <<"join">>}, {<<"name">>, Name}]}, Req, State) ->
	if
		byte_size(Name) < 16, byte_size(Name) > 0 ->
			case chatserver_room:join_room(Name) of
				{ok, ClientList} ->
					Json = jiffy:encode(get_client_list_ejson(ClientList)),
					{reply, {text, Json}, Req, State};
				{error, Reason} ->
					Json = jiffy:encode(get_error_ejson(Reason)),
					{reply, {text, Json}, Req, State}
			end;
		true ->
			Json = jiffy:encode(get_error_ejson(bad_name)),
			{reply, {text, Json}, Req, State}
	end;

%% Handles user messages.
handle_ejson_message({[{<<"type">>, <<"message">>}, {<<"text">>, Text}]}, Req, State) ->
	if
		byte_size(Text) < 256, byte_size(Text) > 0 ->
			chatserver_room:send_message(Text),
			{ok, Req, State};
		true ->
			Json = jiffy:encode(get_error_ejson(bad_message)),
			{reply, {text, Json}, Req, State}
	end;

%% Handles history requests.
handle_ejson_message({[{<<"type">>, <<"get_history">>}]}, Req, State) ->
	Json = jiffy:encode(get_history_ejson(chatserver_hist:retrieve())),
	{reply, {text, Json}, Req, State};

%% Handles unknown messages.
handle_ejson_message(Msg, Req, State) ->
	io:format("Unknown message: ~p~n", [Msg]),
	{ok, Req, State}.

%% Formats messages as JSON.
get_room_message_ejson({message, From, Text, Time}) -> {[
		{type, message},
		{from, From},
		{text, Text},
		{time, Time}
	]};

%% Formats connect messages as JSON.
get_room_message_ejson({connected, User, Time}) -> {[
		{type, user_joined},
		{user, User},
		{time, Time}
	]};

%% Formats disconnect messages as JSON.
get_room_message_ejson({disconnected, User, Time}) -> {[
		{type, user_left},
		{user, User},
		{time, Time}
	]}.

%% Formats errors as JSON.
get_error_ejson(Reason) -> {[
		{type, error},
		{what, Reason}
	]}.

%% Formats the history buffer as JSON.
get_history_ejson(History) -> {[
		{type, history},
		{hist, lists:map(fun get_room_message_ejson/1, History)}
	]}.

%% Formats the client list as JSON.
get_client_list_ejson(ClientList) -> {[
		{type, user_list},
		{list, element(2, lists:unzip(ClientList))}
	]}.
