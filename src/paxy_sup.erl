
-module(paxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Start) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Start]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Start) ->
    %Paxy        = ?CHILD(paxy, worker,Start),
    AcceptorSup = ?CHILD(acceptor_sup, supervisor,Start),
    Receiver    = ?CHILD(receiver, worker, Start),
    Proposer    = ?CHILD(proposer, worker, Start),
    %Acceptor    = ?CHILD(acceptor, worker),
    {ok, { {one_for_one, 5, 10}, [AcceptorSup, Receiver, Proposer]} }.

