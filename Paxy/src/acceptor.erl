-module(acceptor).
-export(start/1).

%% @doc Spawn the init/1 function
%% Spawns:
%% @see init/4
start(Name) ->
    spawn(fun() ->  init(Name) end).

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
                    Proposer ! {promise, Round, Voted, Accepted},
                    acceptor(Name, Round, Voted, Accepted)
                false -> % Already promised in higher ballot
                %% @TODO OPT: not answer
                %% @TODO HELP: send also the promise
                    Proposer ! {sorry, Round},
                    acceptor(Name, Promise, Voted, Accepted)
            end;
        % Round 2
        {accept, Proposer, Round, Proposal} ->
            case order:gr(Round, Promise) of
                true -> % Accept is valid
                   Proposer ! {vote,}, %%Unfinished



end.


