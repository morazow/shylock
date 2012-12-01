%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis
%%% @copyright see LICENSE.txt 
%%% @doc A module which simulates clients to test the behavior of
%%% assigner.
%%% @end
%%% --------------------------------------------------------------------
-module(testc).
-export([start/1]).

-define(delay, 10000).
-define(critical, 10).

%% @doc Initaliazes Number Clients
start(Number) ->
    L = lists:seq(1, Number),
    lists:map(fun(A) -> spawn(fun() -> init(A) end) end ,L).

init(LockID)->
    Name = self(),
    %random:seed(now()),
    %timer:sleep(random:uniform(?delay)),
    comm:send(ass, {decided, LockID, self()}),
    %io:format("Client ~w: Applied for lock with ID ~w~n",[Name, LockID]),
    loop(Name, LockID),
    ok.


loop(Name, LockID) ->
    receive
        get_lock ->
            %io:format("Client ~w: Got lock with ID ~w~n",[Name, LockID]),
            %timer:sleep(random:uniform(?critical)),
            comm:send(ass, {rel_lock, self()}),
            %io:format("Client ~w: Asked to release lock with ID ~w~n",[Name, LockID]),
            loop(Name, LockID);
        rel_lock ->
            %io:format("Client ~w: Release lock with ID ~w~n",[Name, LockID]),
            io:format(" Stop ",[])
    end.
