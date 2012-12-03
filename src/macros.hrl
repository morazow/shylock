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

-ifndef(NODES).
-define(NODES, ['paxy@130.237.20.134', 'paxy@130.237.20.135',
		'paxy@130.237.20.136', 'paxy@130.237.20.137',
		'paxy@130.237.20.138']).
-endif.

-ifndef(ASSIGNOR).
-define(ASSIGNOR, {ass,'paxy@130.237.20.139'}).
-endif.

