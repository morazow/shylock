%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazow
%%% @copyright see LICENSE.txt 
%%% @doc handles an named ets ordered_set table as a FIFO queue
%%% @end
%%% --------------------------------------------------------------------
-module(etsq).
-export([init/1,push/2,pop/1]).

-define(eot, '$end_of_table').


init(Name) ->
    ets:new(Name,[ordered_set,named_table]).

push(Name, Value) ->
    case ets:last(Name) of
        ?eot ->
            ets:insert(Name, {1,Value});
        Last ->
            ets:insert(Name, {Last+1,Value})
    end.

pop(Name) ->% send -1 in case of fail to pop
    case ets:first(Name) of
            ?eot ->
		        timer:sleep(100),
                pop(Name);
            Key ->
            [{_,Result}] = ets:lookup(Name, Key),
            ets:delete(Name,Key),
            Result
    end.
