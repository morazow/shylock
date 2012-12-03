%%% --------------------------------------------------------------------
%%% @author Manos Dimogerontakis, Muhammet Orazow
%%% @copyright see LICENSE.txt 
%%% @doc Library module for transactions with mnesia 
%%% @end
%%% --------------------------------------------------------------------
-module(mnesiaq).
-export([init/2,add/2,read/1]).
-record(slotqueue,{slot, pid}).


init(start, _) -> %%% At the first run
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(slotqueue, [{type,ordered_set},{disc_copies,[node()]},{attributes,record_info(fields,slotqueue)}]);
init(reboot, Nodes) ->
    mnesia:start(),
    mnesia:wait_for_tables(slotqueue,500),
    LocalMax = mnesia:dirty_last(slotqueue),
    {Max,MaxNode} = findDMax(Nodes),
    io:format("MaxNode: ~w  Max: ~w~n",[MaxNode,Max]),
    io:format("LocalMax: ~w~n",[LocalMax]),
    MatchHead = #slotqueue{slot='$1',pid ='$2'},
    Guard = {'>', '$1', LocalMax},
    Result = ['$1','$2'],
    Args = [slotqueue,[{MatchHead, [Guard], [Result]}]],
    Res = rpc:call(MaxNode,mnesia,dirty_select,Args),
    lists:map(fun([A,B]) -> add(A,B) end,Res).

%init(reboot, Node) -> %%% If node reboots
                %%% @TODO OPT: For now copy all entries since learner is not 
                %%% a bottleneck at the same time we can now the bigest slot 
                %%% number and send to proposer so he can act
                %%% This can be done also with RPC call
                %%% http://stackoverflow.com/questions/2378281/mnesia-reading-remote-node-data-in-local-content-true-mode
    %%% Retrieve remote copy
%    mnesia:start(),
%    mnesia:change_config(extra_db_nodes, [Node]),
%    mnesia:change_table_copy_type(schema, node(), disc_copies),
    %%% Find greater slot and send to proposer
%    io:format("Mnesiaq: Sending to proposer~n"),
%    Max=lists:max(mnesia:dirty_all_keys(slotqueue)),
%    whereis(proposer) ! {max_key,Max},
%    io:format("Sent to proposer~n"),
    %%% Copy locally the table
%    Tabs = mnesia:system_info(tables) -- [schema],
%    io:format("Read Tabs~n"),
%    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

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

findDMax(Nodes) ->
    Lasts = lists:map(fun(A) -> {rpc:call(A,mnesia,dirty_last,[slotqueue]),A} end,Nodes),
    lists:max(Lasts).
