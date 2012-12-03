%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazow
%%% @copyright see LICENSE.txt 
%%% @doc Library module for transactions with mnesia 
%%% @end
%%% --------------------------------------------------------------------
-module(mnesiaq).
-export([init/1,add/2,read/1]).
-record(slotqueue,{slot, pid}).


init(start) -> %%% At the first run
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(slotqueue, [{disc_copies,[node()]},{attributes,record_info(fields,slotqueue)}]);
init(reboot) -> %%% If node reboots
                %%% @TODO OPT: For now copy all entries since learner is not 
                %%% a bottleneck at the same time we can now the bigest slot 
                %%% number and send to proposer so he can act
    %%% Retrieve remote copy
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    %%% Find greater slot and send to proposer
    Max=lists:max(mnesia:all_keys(slotqueue)),
    proposer ! {max_key,Max},
    %%% Copy locally the table
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

add(Slot, PID) ->
    F = fun() -> 
            mnesia:write(#slotqueue{slot=Slot,pid=PID})
        end,
    mnesia:activity(transaction,F).

read(Slot) ->
    F = fun() ->
            case mnesia:read({slotqueue, Slot}) of
                [] ->
                    {error, unknown_slot};
                [Ans] ->
                    {_,_,PID}=Ans,
                    PID
            end
        end,
    mnesia:activity(transaction, F).
