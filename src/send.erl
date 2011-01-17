%% Copyright Richard Alexander Green 2011
%% Created: Jan 4, 2011
%% Description: Utility module to facilitate distributed application.
%% -- IMO, this should be built-into ! operator -- but it is not
-module(send).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([send/2]).

%%
%% API Functions
%%

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

%%
%% Local Functions
%%

