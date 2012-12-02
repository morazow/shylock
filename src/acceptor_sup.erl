%%%-------------------------------------------------------------------
%%% @author manos, morazow
%%% @copyright (C) 2012,
%%% @doc
%%% Supervisor for acceptors
%%% @end
%%%-------------------------------------------------------------------
-module(acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Start) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Start]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Start) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    % CHECK: do we need names?
    Acceptor = {acceptor, {acceptor, start, [Start]},
	      Restart, Shutdown, Type, [acceptor]},

    {ok, {SupFlags, [Acceptor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
