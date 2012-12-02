%%%-------------------------------------------------------------------
%%% @author manos, morazow
%%% @copyright (C) 2012,
%%% @doc
%%% Paxy server, keeps global view of acceptors
%%% @end
%%%-------------------------------------------------------------------
-module(paxy).

-behaviour(gen_server).

-include("macros.hrl").

%% API
-export([start_link/0, stop/0]).
-export([register_acceptor/1, get_acceptors/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {acceptors = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_acceptor(Pid) ->
    gen_server:call(?SERVER, {register_acceptor, Pid}).

get_acceptors() ->
    gen_server:call(?SERVER, get_acceptors).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    % connect to other Paxies
    [spawn(fun() -> net_adm:ping(list_to_atom(Node)) end) || Node <- ?NODES],
    State = #state{acceptors = []},
    {ok, State}.

handle_call({register_acceptor, Pid}, _From, State) ->
    Reply = ok,
    {reply, Reply, handle_register_acceptors(Pid, State)};

handle_call(get_acceptors, _From, State) ->
    Reply = State#state.acceptors,
    {reply, Reply, State}.

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
handle_register_acceptors(Pid, #state{acceptors=[Rest]}=State) ->
    NewAcc   = [Pid | Rest],
    NewState = State#state{acceptors = NewAcc},
    NewState.
