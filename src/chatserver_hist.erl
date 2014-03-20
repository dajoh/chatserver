-module(chatserver_hist).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	append/1,
	retrieve/0
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

append(Msg) ->
	gen_server:cast(whereis(?SERVER), {append, Msg}).

retrieve() ->
	gen_server:call(whereis(?SERVER), {retrieve}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, []}.

handle_call({retrieve}, _From, State) ->
	{reply, State, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({append, Msg}, [_|State]) when length(State) =:= 199 ->
	{noreply, State ++ [Msg]};
handle_cast({append, Msg}, State) ->
	{noreply, State ++ [Msg]};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
