%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis
%%% @copyright see LICENSE.txt 
%%% @doc assignor is responsible to assign and release the locks
%%% Therefore he needs to hold two queues: On of the client who
%%% are able to get the lock and one of the clients who asked to
%%% release it.
%%% @end
%%% --------------------------------------------------------------------
-module(assignor).
-export([start/0]).


%% @doc Start module with registered name ass.
%% Spawns:
%% @see init/1
start() ->
    register(ass, spawn(fun() -> init(ass) end)).

%% @doc Initialized gb_tree structure.
%% Calls:
%% @see loop/4
init(Name) ->
    T = gb_trees:empty(),
    loop(Name,T, 0, null).


%% @doc Main loop function. Either we do not know yet the PID  of the 
%%      next client to take the lock and call loopget/5 or we find him
%%      in the tree, send him the lock and call looprel/4
%% Calls
%% @see loopget/5
%% @see looprel/4
loop(Name, T, LockID, LockHolder) ->
    NextLockID = LockID+1,
    case gb_trees:lookup(NextLockID,T) of
        none ->
            loopget(Name, T, LockID, LockHolder, NextLockID);
        {_,NextHolder} ->% We assume no gaps in the decisions
            %io:format("Module Assignor: sending get_lock: ~w~n",[NextHolder]),
            comm:send(NextHolder, get_lock),
            T1 = gb_trees:delete(NextLockID, T),
            looprel(Name, T1, NextLockID, NextHolder)
    end.


%% @doc Waits for the PID of the next slot to arrive to send it the get_lock
%% Calls
%% @see looprel/4
loopget(Name, T, LockID, LockHolder, NextLockID) ->
    receive 
        {decided, NextLockID, Client} ->
            comm:send(Client, get_lock),
            looprel(Name, T, LockID+1, Client);
        {decided, Slot, Client} ->
            T1 = gb_trees:enter(Slot, Client, T),
            loopget(Name, T1, LockID, LockHolder, NextLockID);
        Msg ->
            io:format("Module ~w: Unexpected message in loopget : ~w",[?MODULE,Msg]),
            loopget(Name, T, LockID, LockHolder, NextLockID)
    end.

%% @doc Waits for the lock holder to send rel_lock and and sends to it a
%%      message to confirm the the lock was released (rel_lock)
%% Calls
%% @see loop/4
looprel(Name, T, LockID, LockHolder) ->
    receive
        {rel_lock, LockHolder} -> % We are sure that only one rel_lock
                              % will come at a time since we have only
                              % one lock
            comm:send(LockHolder, rel_lock),
            loop(Name, T, LockID, LockHolder);
        {rel_lock, _} ->
            %io:format("Module ~w: Wrong rel_lock. ~nThere is sth really WRONG in your system",[?MODULE]),
            looprel(Name, T, LockID, LockHolder);
        {decided, Slot, Client} ->
            T1 = gb_trees:enter(Slot, Client, T),
            looprel(Name, T1, LockID, LockHolder);
        Msg -> 
            io:format("Module ~w: Unexpected message in looprel : ~w",[?MODULE,Msg]),
            looprel(Name, T, LockID, LockHolder)
    end.
