%%%-------------------------------------------------------------------
%%% @author morazow, manos
%%% @copyright (C) 2012,
%%% @doc
%%% Simple lock server with queue, serve single lock to clients.
%%% Requesting clients wait for lock.
%%% @end
%%%-------------------------------------------------------------------
-module(lock_server_queue).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([acquire_lock/1, release_lock/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {queue = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire_lock(ClientId) ->
    gen_server:call(?SERVER, {acquire_lock, ClientId}).

release_lock(ClientId) ->
    gen_server:call(?SERVER, {release_lock, ClientId}).

stop() ->
    gen_server:call(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{ queue = queue:new()}}.

handle_call({acquire_lock, ClientId}, _From, State) ->
    {reply, ok, handle_acquire_lock(ClientId, State)};
handle_call({release_lock, ClientId}, _From, State) ->
    {reply, ok, handle_release_lock(ClientId, State)}.


handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_acquire_lock(ClientId, #state{queue=Q0}=State) ->
    Q1 = queue:in(ClientId, Q0),
    case queue:is_empty(Q0) of
	true ->
	    send_message(ClientId);
	false ->
	    noop
    end,
    State#state{queue=Q1}.

handle_release_lock(_, State) ->
    case queue:out(State#state.queue) of
	{{value, _PrevClient}, Q} ->
	    case queue:peek(Q) of
		{value, NextClient} ->
		    send_message(NextClient);
		empty ->
		    ok
	    end,
	    State#state{queue=Q};
	{empty, Q0} ->
	    State#state{queue=Q0}
    end.

send_message(Pid) ->
    spawn(fun() -> Pid ! lock end).

