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
	{ok, Req, no_name}.

%% Called when a client sets it's name.
websocket_handle({text, Name}, Req, no_name) ->
	chatserver_room:change_name(Name),
	{ok, Req, {has_name, Name}};

%% Called when a client sends a client list command.
websocket_handle({text, <<"/users">>}, Req, State) ->
	%% Retrive the client list and get a list of names
	Clients = chatserver_room:get_client_list(),
	{_, BinNames} = lists:unzip(Clients),

	%% Convert names to strings and build the reply
	StrNames = [binary_to_list(X) || X <- BinNames],
	Reply = "Users in this chat: " ++ string:join(StrNames, ", "),

	{reply, {text, Reply}, Req, State};

%% Called when a client sends a chat message.
websocket_handle({text, Msg}, Req, State) ->
	chatserver_room:broadcast_message(Msg),
	{ok, Req, State};

%% Called when we get an unknown message.
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

%% Called on broadcasts from other users.
websocket_info({message, Name, Msg}, Req, State) ->
	{reply, {text, [Name, ": ", Msg]}, Req, State};

%% Called on unknown system messages.
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

%% Called when clients disconnect.
websocket_terminate(_Reason, _Req, _State) ->
	ok.
