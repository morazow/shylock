
-module(shylock_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-include("sup.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Start) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Start]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Start) ->
    Receiver    = {receiver, {receiver, start_link, []},?RESTART, ?SHUTDOWN, ?TYPE, [receiver]},
    AcceptorSup = ?CHILD(acceptor_sup, supervisor, Start),  
    ProposerSup = ?CHILD(proposer_sup, supervisor, Start),
    %LearnerSup  = ?CHILD(learner_sup, supervisor, Start),
    
    {ok, { {one_for_one, 5, 10}, [Receiver, AcceptorSup, ProposerSup]} }.

