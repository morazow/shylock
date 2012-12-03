%%%-------------------------------------------------------------------
%%% @author manos, morazow
%%% @copyright (C) 2012,
%%% @doc
%%% Supervisor for learners
%%% @end
%%%-------------------------------------------------------------------
-module(learner_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("sup.hrl").

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
    Learner = {learner, {learner, start, Start}, ?RESTART, ?SHUTDOWN, ?TYPE, [learner]},
    {ok, {?SUPFLAGS, [Learner]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
