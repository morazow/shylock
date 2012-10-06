-module(proposer).
-export(start/4).

%% @doc 3 main parameters:</br>
%% <ul>
%% <li><emph>timeout</emph>: max milliseconds to wait for accept and 
%%     vote messages for </li>
%% <li><emph>backoff</emph>: increasing number of milliseconds between
%%     rounds</li>
%% <li><emph>delay</emph>: introduce delay to the system</li>
%% </ul>

-define(timeout , 2000).
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
    random:seed(Seed, Seed, Seed,),
    Round = order:null(),
    round(Name, ?backoff, Round, Proposal, Acceptors).

%% @doc A whole round where:</br>
%% <ul>
%% <li><emph>1st comm round</emph>: Proposer sends <emph>prepare</emph>, 
%%     waits for majority of <emph>promise</emph> messages</li>
%% <li><emph>2nd comm round</emph>: Proposer send <emph>accept</emph> 
%%     to everyone, waits for majority of <emph>vote</emph> messages</li>
%% </ul></br>
%% Calls:
%% @see ballot/3
round(Name, Backoff, Round, Proposal, Acceptors) ->
    case ballot(Round, Proposal, Acceptors) of
        {ok,Decision} -> % A majority quorum agreed to accept
            io:format("~w decided ~w  in round ~w~n",[Name,Decision,Round]),
            {ok,}; %%Unfinished
        abort -> % No majority due to timeout 
            timer:sleep(random:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name,(2*Bakcoff),Next,Proposal,Acceptors)
    end.







%% @doc
%% Calls:
%% @see prepare/2
%% @see collect/4
ballot(Round, Proposal, Acceptors) ->
    prepare(,), %Unfinished
    Quorum = (lenght(Acceptors) div 2) + 1,
    Max = order:null(),
    case collect(Quorum, Round, Max, Proposal) of
        {accepted,Value} ->




    end.


%% @doc
%% <ul>
%% <li><emph>max</emph>: highest sequence number accepted
%%     by a member of the quorum</li>
%% </ul>
%% Possible Problem:</br>
%% We reassure that we return somethin only when majority exists
%% but how do we reassure that we are going also to retunr a 
%% correct value (possible conflicting promises with na and value?)
collect(0, _, _, Proposal) ->
    {accepted,Max};%%Unfinished
collect(N, Round, Max, Proposal) ->
    receive %%We collect only messages referring to this round
    %% @TODO maybe optimize message discaring using flush/0
        {promise,Round,_, na} ->
            collect(N-1,Round,);%%Unfinished
        {promise,Round,Voted, Accepted} ->

            ;
        {promise,_,_,_} ->
            collect(N, Round, Max, Proposal);
        {sorry,Round} ->
            collect(N,Round, Max, Proposal);
        {sorry,_} ->
            collect(N,Round, Max, Proposal)
    after ?timeout ->
            abort
    end.


prepare(Round, Acceptors) -> 
    multicast({prepare,self(),Round},Acceptors).

accept(Round, Proposal, Acceptors) -> 
    multicast({accept,self(),Round,Proposal}, Acceptors).

multicast(Msg, Dests) ->
    lists:map(fun(Dest) -> send(Dest,Msg) end,Dests).

send(Name, Message) -> 
    Name ! Message.

 
