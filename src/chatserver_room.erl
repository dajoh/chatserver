-module(chatserver_room).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	change_name/1,
	get_client_list/0,
	broadcast_message/1
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

change_name(Name) ->
	gen_server:call(whereis(?SERVER), {change_name, Name}).

get_client_list() ->
	gen_server:call(whereis(?SERVER), {get_client_list}).

broadcast_message(Msg) ->
	gen_server:cast(whereis(?SERVER), {broadcast_message, self(), Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% Called on init
init(_Args) ->
	{ok, dict:new()}.

%% Called on new client
handle_call({change_name, Name}, {Pid, _}, State) ->
	%% Find out if it's a new client or old one changing name
	case dict:find(Pid, State) of
		{ok, OldName} -> send_to_all({changed_name, OldName, Name}, State);
		error         -> send_to_all({connected, Name}, State), monitor(process, Pid)
	end,
	%% Update client list
	{reply, ok, dict:store(Pid, Name, State)};

%% Called on client list request
handle_call({get_client_list}, _From, State) ->
	{reply, dict:to_list(State), State};

%% Called on unknown call
handle_call(_Request, _From, State) ->
	{reply, unknown_call, State}.

%% Called on client message
handle_cast({broadcast_message, Pid, Msg}, State) ->
	{ok, Name} = dict:find(Pid, State),
	send_to_all({message, Name, Msg}, State),
	{noreply, State};

%% Called on unknown cast
handle_cast(_Msg, State) ->
	{noreply, State}.

%% Called on client disconnect
handle_info({'DOWN', _, process, Pid, _}, State) ->
	send_to_all({disconnected, dict:fetch(Pid, State)}, State),
	{noreply, dict:erase(Pid, State)};

%% Called on unknown system message
handle_info(_Info, State) ->
	{noreply, State}.

%% Called on terminate
terminate(_Reason, _State) ->
	ok.

%% Called on version change
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_to_all(Msg, Clients) ->
	dict:map(fun (K, V) ->
		K ! Msg,
		{K, V}
	end, Clients).
