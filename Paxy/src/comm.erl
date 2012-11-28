-module(comm).
-export([mcast/2,send/2]).
-include("macros.hrl").

mcast(Msg, Dests) ->
    lists:map(fun(Dest) -> send (Dest,Msg) end, Dests).

send(Name, Message) ->
    Name ! Message.

    
