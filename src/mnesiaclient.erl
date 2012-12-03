%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazow
%%% @copyright see LICENSE.txt 
%%% @doc A module which simulates clients to test the behavior of
%%% mnesia.
%%% @end
%%% --------------------------------------------------------------------
-module(mnesiaclient).
-export([start/0]).

start() ->
    %loop().
    register(mc, spawn(fun() -> loop() end)).

loop() ->
    receive
        {init, Bool} ->
            mnesiaq:init(Bool);
        {add, Slot, PID} ->
            mnesiaq:add(Slot,PID);
        {read, Slot} ->
            io:format("Slot: ~w~n",[Slot]),
            PID = mnesiaq:read(Slot),
            io:format("Slot: ~w, PID:  ~w~n",[Slot,PID])
    end,
    loop().
