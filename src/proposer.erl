%%% @doc 3 main parameters:
%% <ul>
%% <li><emph>timeoutpre</emph>: max milliseconds to wait for accept messages</li>
%% <li><emph>timeoutpre</emph>: max milliseconds to wait for vote messages</li>
%% <li><emph>backoff</emph>: increasing number of milliseconds between
%%     rounds</li>
%% <li><emph>delay</emph>: introduce delay to the system</li>
%% </ul>
-module(proposer).
-export([start/4]).
-include("macros.hrl").


-define(timeoutprep , 2000).
-define(timeoutvote , 2000).
-define(backoff , 10).
-define(delay , 20).

%% @doc Spawn the init/4 function
%% Spawns:
%% @see init/4
start(Name, Proposal, Acceptors, Seed) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Seed) end).
    
%% @doc Call the main proposer loop
%% Calls:
%% @see round/5
init(Name, Proposal, Acceptors, Seed) ->
    %% @TODO v4: acceptors should change dynamically
    %%       v1a: proposal is the incoming message    
    random:seed(Seed, Seed, Seed),
    Round = order:null(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors).

%% @doc A whole round where:
%% <ul>
%% <li><emph>1st comm round</emph>: Proposer sends <emph>prepare</emph>, 
%%     waits for majority of <emph>promise</emph> messages</li>
%% <li><emph>2nd comm round</emph>: Proposer send <emph>accept</emph> 
%%     to everyone, waits for majority of <emph>vote</emph> messages</li>
%% </ul>
%% <emph>Round</emph>: (RoundNumber,Name)
%% Calls:
%% @see ballot/4
round(Name, Backoff, Round, Proposal, Acceptors) ->
    case ballot(Round, Proposal, Acceptors, Name) of
        {ok,Decision} -> % A majority quorum agreed to accept
            io:format("~w decided ~w  in round ~w~n",[Name,Decision,Round]),
            {ok,Decision};
        abort -> % No majority due to timeout 
            timer:sleep(random:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name,(2*Backoff),Next,Proposal,Acceptors)
    end.




%% @doc
%% Calls:
%% @see prepare/2
%% @see collect/4
%% @see vote/2
ballot(Round, Proposal, Acceptors, Name) ->
    io:format("~w [Proposer ~w] Phase 1~n",[now(),Name]),
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    % @ TODO OPT: if i can save which is the quorum i can
    %             ask send then accepts msgs only to them
    Max = order:null(),
    case collect(Quorum, Round, Max, Proposal) of
        {accepted,Value} -> % IN V3 I WOULD THE LEADER, MOVE TO PHASE 2
            % @TODO v4: Acceptors/quorum number may be changed
            %          everytime phase two is run
            io:format("~w [Proposer ~w] Phase 1 Majority! Value: ~w~n",[now(), Name,Value]),
            accept(Round, Value, Acceptors),
            case vote(Quorum, Round) of
                ok ->
                    io:format("~w [Proposer ~w] Phase 2 Majority! Value: ~w~n",[now(), Name,Value]),
                    {ok,Value};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.


%% @doc
%% <ul>
%% <li><emph>max</emph>: highest sequence number accepted
%%     by a member of the quorum</li>
%% </ul>
%% Possible Problem:
%% We reassure that we return something only when majority exists
%% but how do we reassure that we are going also to return a 
%% correct value (possible conflicting promises with na and value?)
collect(0, _, _, Proposal) ->
    {accepted,Proposal};
collect(N, Round, Max, Proposal) ->
    receive %%We collect only messages referring to this round
    %% @TODO maybe optimize message discaring using flush/0
        {promise,Round,Voted, na} ->
            case Voted /= order:null() of
                true -> 
                    ?DEBUG("Acceptor Voted but accepted value is na"),
                    collect(N-1,Round,Max,Proposal);
                false ->
                    collect(N-1,Round,Max,Proposal)
            end;
        {promise,Round,Voted, Accepted} ->
            case order:gr(Voted,Max) of % we send Voted to make this
                                        % comparison
                true -> % change to highest proposal to ensure P2c
                    collect(N-1, Round, Voted, Accepted);
                false -> 
                    collect(N-1, Round, Max, Proposal)
            end;
        {promise,_,_,_} -> %% Clear the rest of the promises that
                           %% we do not care about.
                           %% If we implement Multi-Paxos we have
                           %% to change that
            collect(N, Round, Max, Proposal);
        {sorry,Round} -> %% @TODO OPT: count sorries in case they reach
                         %%            majority and we can abort
            collect(N,Round, Max, Proposal);
        {sorry,_} ->
            collect(N,Round, Max, Proposal)
    after ?timeoutprep ->
            abort
    end.

vote(0,_) ->
    ok;
vote(N, Round) ->
    receive
        {vote,Round} ->
            vote(N-1,Round);
        {vote, _} ->
            vote(N,Round);
        {sorry,Round} -> %%TODO OPT: count sorry answers in case they 
                         %%         reach a majority and we abort
            vote(N,Round);
        {sorry, _} -> 
            vote(N,Round)
    after ?timeoutvote ->
            abort
    end.


prepare(Round, Acceptors) -> 
    %% @TODO v2 Here it should be sth else, i.e. an erlang
    %%       reference, as we will assume stable names
    %%       for the proposers and the communication will
    %%       happen through different nodes
    comm:mcast({prepare,self(),Round},Acceptors).


accept(Round, Proposal, Acceptors) ->
    %% @TODO v2 Here it should be sth else, i.e. an erlang
    %%       reference, as we will assume stable names
    %%       for the proposers and the communication will
    %%       happen through different nodes
    comm:mcast({accept,self(),Round,Proposal}, Acceptors).

