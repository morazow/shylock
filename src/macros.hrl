%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis
%%% @copyright see LICENSE.txt 
%%% --------------------------------------------------------------------
-ifndef(DEBUGVar).
-define(DEBUGVar(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE,??Var, Var])).
-endif.

-ifndef(DEBUG).
-define(DEBUG(Msg), io:format("DEBUG: ~p:~p - ~p~n~n", [?MODULE, ?LINE,Msg])).
-endif.

-define(NODES, ['paxos@130.237.20.134', 'paxos@130.237.20.135',
		'paxos@130.237.20.136', 'paxos@130.237.20.137',
		'paxos@130.237.20.138']).

-define(ASSIGNOR, {ass,'paxos@130.237.20.139').
