%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis
%%% @copyright see LICENSE.txt 
%%% --------------------------------------------------------------------


-ifndef(RESTRAT).
-define(RESTRAT,one_for_one).
-endif.


-ifndef(MAXRESTARTS).
-define(MAXRESTARTS, 1000).
-endif.


-ifndef(MSBR).
-define(MSBR,3600).
-endif.


-ifndef(SUPFLAGS).
-define(SUPFLAGS,{?RESTRAT,?MAXRESTARTS,?MSBR}).
-endif.


-ifndef(RESTART).
-define(RESTART,permanent).
-endif.


-ifndef(SHUTDOWN).
-define(SHUTDOWN,2000).
-endif.

-ifndef(TYPE).
-define(TYPE,worker).
-endif.

-ifndef(SERVER).
-define(SERVER,?MODULE).
-endif.

-ifndef(CHILD).
-define(CHILD(I, Type, Var), {I, {I, start_link, Var}, ?RESTART, ?SHUTDOWN, Type, [I]}).
-endif.
