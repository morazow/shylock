%%%-------------------------------------------------------------------
%%% @author manos, morazow
%%% @doc
%%% Receiver module, queue the clients and starts paxos runs
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).

-behaviour(gen_server).

%% API
-export([start_link/0, acquire_lock/1, release_lock/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire_lock(ClientPid) ->
    gen_server:call(?SERVER, {acquire_lock, ClientPid}).

release_lock(ClientPid) ->
    gen_server:call(?SERVER, {release_lock, ClientPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("starting receiver ~n"),
    etsq:init(clients),
    {ok, #state{}}.

handle_call({acquire_lock, ClientPid}, _From, State) ->
    io:format("received message ~n"),
    Reply = ok,
    {reply, Reply, handle_acquire(ClientPid, State)};
handle_call({release_lock, ClientPid}, _From, State) ->
    Reply = ok,
    {reply, Reply, handle_release(ClientPid, State)}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({acquire_lock, Pid}, State) ->
    io:format("got fucking a message ~w~n", [Pid]),
    {noreply, handle_acquire(Pid, State)};
handle_info({release_lock, Pid}, State) ->
    io:format("got fucking a message ~w~n", [Pid]),
    {noreply, handle_release(Pid, State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_acquire(Pid, State) ->
    % add Pid to state
    etsq:push(clients, Pid),
    State.

handle_release(_Pid, State) ->
    % forward release to assigner
    etsq:pop(clients), % I did not get how pop, works here
                      % not sure whether we should pop here or in assigner
    State.
