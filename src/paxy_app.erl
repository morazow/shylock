-module(paxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, reboot/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    paxy_sup:start_link(start).

reboot(_StartType, _StartArgs) ->
    paxy_sup:start_link(reboot).

stop(_State) ->
    ok.
