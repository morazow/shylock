-ifndef(DEBUGVar).
-define(DEBUGVar(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE,??Var, Var])).
-endif.

-ifndef(DEBUG).
-define(DEBUG(Msg), io:format("DEBUG: ~p:~p - ~p~n~n", [?MODULE, ?LINE,Msg])).
-endif.


