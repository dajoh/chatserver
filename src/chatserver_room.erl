-module(chatserver_room).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	join_room/1,
	send_message/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join_room(Name) ->
	gen_server:call(whereis(?SERVER), {join_room, Name}).

send_message(Text) ->
	gen_server:cast(whereis(?SERVER), {send_message, self(), Text}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, {dict:new(), dict:new()}}.

handle_call({join_room, Name}, {Pid, _}, {PidToName, NameToPid}) ->
	case dict:is_key(Pid, PidToName) of
		true ->
			{reply, {error, already_in}, {PidToName, NameToPid}};
		false ->
			case dict:is_key(Name, NameToPid) of
				true ->
					{reply, {error, name_taken}, {PidToName, NameToPid}};
				false ->
					monitor(process, Pid),
					broadcast({connected, Name}, PidToName),
					NewPidToName = dict:store(Pid, Name, PidToName),
					NewNameToPid = dict:store(Name, Pid, NameToPid),
					{reply, {ok, dict:to_list(NewPidToName)}, {NewPidToName, NewNameToPid}}
			end
	end;
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_message}, State}.

handle_cast({send_message, Pid, Text}, {PidToName, NameToPid}) ->
	case dict:is_key(Pid, PidToName) of
		true ->
			broadcast({message, dict:fetch(Pid, PidToName), Text}, PidToName);
		false ->
			omg_plz
	end,
	{noreply, {PidToName, NameToPid}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, {PidToName, NameToPid}) ->
	Name = dict:fetch(Pid, PidToName),
	broadcast({disconnected, Name}, PidToName),
	{noreply, {
		dict:erase(Pid, PidToName),
		dict:erase(Name, NameToPid)
	}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

broadcast(Msg, PidToName) ->
	dict:map(fun (Pid, _) -> Pid ! Msg end, PidToName).
