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
	{ok, Req, []}.

%% Called when we get a JSON message.
websocket_handle({text, Json}, Req, State) ->
	try jiffy:decode(Json) of
		{[{<<"type">>, <<"join">>}, {<<"name">>, Name}]} ->
			if
				byte_size(Name) < 16, byte_size(Name) > 0 ->
					case chatserver_room:join_room(Name) of
						{ok, ClientList} ->
							{reply, {text, get_client_list_json(ClientList)}, Req, State};
						{error, Reason} ->
							{reply, {text, get_error_json(Reason)}, Req, State}
					end;
				true ->
					{reply, {text, get_error_json(bad_name)}, Req, State}
			end;
		{[{<<"type">>, <<"message">>}, {<<"text">>, Text}]} ->
			if
				byte_size(Text) < 256, byte_size(Text) > 0 ->
					chatserver_room:send_message(Text),
					{ok, Req, State};
				true ->
					{reply, {text, get_error_json(bad_message)}, Req, State}
			end;
		{[{<<"type">>, <<"get_history">>}]} ->
			{reply, {text, get_history_json()}, Req, State};

		_ -> {shutdown, Req, State} %% unknown message
	catch
		_ -> {shutdown, Req, State} %% fuq the police
	end;

%% Called when we get an unknown message.
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

%% Called on broadcasts from other users.
websocket_info({message, Name, Msg, Time}, Req, State) ->
	Json = jiffy:encode({[
		{type, message},
		{from, Name},
		{text, Msg},
		{time, Time}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on connections from other users.
websocket_info({connected, Name, Time}, Req, State) ->
	Json = jiffy:encode({[
		{type, user_joined},
		{user, Name},
		{time, Time}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on disconnects from other users.
websocket_info({disconnected, Name, Time}, Req, State) ->
	Json = jiffy:encode({[
		{type, user_left},
		{user, Name},
		{time, Time}
	]}),
	{reply, {text, Json}, Req, State};

%% Called on unknown system messages.
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

%% Called when clients disconnect.
websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% Formats the client list as JSON.
get_client_list_json(ClientList) ->
	{_, Names} = lists:unzip(ClientList),
	jiffy:encode({[
		{type, user_list},
		{list, Names}
	]}).

%% Formats errors as JSON.
get_error_json(Reason) ->
	jiffy:encode({[
		{type, error},
		{what, Reason}
	]}).

%% Formats the history buffer as JSON.
get_history_json() ->
	History = lists:map(
		fun ({message, From, Text, Time}) ->
				{[{type, message}, {from, From}, {text, Text}, {time, Time}]};
			({connected, User, Time}) ->
				{[{type, user_joined}, {user, User}, {time, Time}]};
			({disconnected, User, Time}) ->
				{[{type, user_left}, {user, User}, {time, Time}]}
	end, chatserver_hist:retrieve()),
	jiffy:encode({[
		{type, history},
		{hist, History}
	]}).
