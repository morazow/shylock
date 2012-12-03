%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazow
%%% @copyright see LICENSE.txt 
%%% --------------------------------------------------------------------
-module(learner).
-export([start/1]).
-include("macros.hrl").


start(Start) ->
    register(learner,spawn(fun() -> init(Start) end )),
    {ok,self()}.

init(start) ->
    mnesiaq:init(start, null),
    loop();
init(reboot) ->
    Nodes = lists:delete(node(),?NODES),
    mnesiaq:init(reboot, lists:map(fun(A) -> {learner,A} end ,Nodes)),
    loop().

loop() ->
    receive
        {decided, Slot, Pid} ->
            spawn(fun() ->  mnesiaq:add(Slot,Pid) end),
            comm:send(?ASSIGNOR,{decided,Slot,Pid}),
            loop()
    end.
