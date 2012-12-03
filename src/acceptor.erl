%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazov, Johan Montelius
%%% @copyright see LICENSE.txt 
%%% --------------------------------------------------------------------
-module(acceptor).
-export([start/1]).
-include("macros.hrl").

%% @doc Spawn the init/1 function
%% Spawns:
%% @see init/4
start(Start) ->
    io:format("acceptor: ~w~n",[Start]),
    case Start of 
        start ->
            register(acceptor, spawn(fun() -> init(a) end)),
            % send this Pid to everyone
            %[spawn(fun() -> rpc:call(list_to_atom(Node), paxy, register_acceptor, [Pid]) end) || Node <- ?NODES],
            {ok, self()}; % Instead of self() we can put whereis(acceptor) and
                          % supervise the spawned function
        reboot ->
            reboot
    end.

%% @doc Call the main acceptor loop
%% Calls:
%% @see acceptor/4
init(Name) ->
    Promise = order:null(),
    Voted = order:null(),
    Accepted = na,
    acceptor(Name, Promise, Voted, Accepted).
    
%% @doc 
%% <ul>
%% <li><emph>Promise</emph>: promised not to accept any ballot below
%%     this number</li>
%% <li><emph>Voted</emph>: the highest ballot number accepted</li>
%% <li><emph>Accepted</emph>: the value that has been accepted</li>
%% </ul>
acceptor(Name, Promise, Voted, Accepted) ->
    receive
        % Round 1
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promise) of
                true -> % Able to make the promise
                    comm:send(Proposer,{promise, Round, Voted, Accepted}),
                    acceptor(Name, Round, Voted, Accepted);
                false -> % Already promised in higher ballot
                %% @TODO OPT: not answer
                %% @TODO HELP: send also the promise
                    comm:send(Proposer,{sorry, Round}),
                    acceptor(Name, Promise, Voted, Accepted)
            end;
        % Round 2
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promise) of
                true -> % Accept is valid
                    comm:send(Proposer,{vote,Round}), %%Unfinished
                    %% @TODO v2: Here the learner should be triggered
                    case order:goe(Round, Voted) of
                        true ->
                            acceptor(Name, Promise, Round, Proposal);
                        false ->
                            acceptor(Name, Promise, Voted, Accepted)
                    end;
                false ->
                    comm:send(Proposer,{sorry,Round}),
                    acceptor(Name, Promise, Voted, Accepted)
            end;
        stop ->
            ok
    end.


