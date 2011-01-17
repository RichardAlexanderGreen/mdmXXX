%% Author: admin
%% Created: Dec 6, 2010
%% Description: TODO: Add description to mdm_server_tests
-module(mdm_server_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

% { service, S, device, D, reports_usage, U, for_period, P }

cast_usage_test() ->
		T1 = now(),
		timer:sleep( 100 ),
		T2 = now(),
		%MDM = whereis( mdm_server ),
		%MDM ! { service, "test_service", device, "test_device", reports_usage, {123, watt_hour}, for_period, { T1, T2 } }.
		mdm_server:cast( { service, "test_service", device, "test_device", reports_usage, {123, watt_hour}, for_period, { T1, T2 } } ).



%%
%% Local Functions
%%

