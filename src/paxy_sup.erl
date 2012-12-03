
-module(paxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Start), {I, {I, start_link, Start}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Start) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Start]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Start) ->
    %Paxy        = ?CHILD(paxy, worker,Start),
    AcceptorSup = ?CHILD(acceptor_sup, supervisor, Start),
    %Receiver    = ?CHILD(receiver, worker),
    %Proposer    = ?CHILD(proposer, worker, Start),
    %Acceptor    = ?CHILD(acceptor, worker),
    {ok, { {one_for_one, 5, 10}, [AcceptorSup]} }.

