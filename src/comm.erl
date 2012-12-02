%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis
%%% @copyright see LICENSE.txt 
%%% --------------------------------------------------------------------
-module(comm).
-export([mcast/2,send/2]).
-include("macros.hrl").

mcast(Msg, Dests) ->
    lists:map(fun(Dest) -> send (Dest,Msg) end, Dests).


nmcast(Msg, Process) ->
    lists:map(fun(Dest) -> send ({Process,Dest},Msg) end, ?NODES).


send(Name, Message) ->
    Name ! Message.


