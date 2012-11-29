-module(paxy).
-export([start/1,stop/0, stop/1]).


start(Seed) ->
    register(a, acceptor:start(a)),
    register(b, acceptor:start(b)),
    register(c, acceptor:start(c)),
    register(d, acceptor:start(d)),
    register(e, acceptor:start(e)),
    Acceptors = [a,b,c,d,e],
    proposer:start(kurtz, green, Acceptors, Seed+1),
    proposer:start(willard, red, Acceptors, Seed+2),
    proposer:start(kilgore, blue, Acceptors, Seed+3),
    true.

stop() ->
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e).

stop(Name) ->
    case whereis(Name) of
        defined ->
            ok;
        Pid ->
            Pid ! stop
    end.
