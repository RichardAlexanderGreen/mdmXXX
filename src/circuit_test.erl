%% Copyright: Richard Alexander Green 2011
%% Description: Test circuit module.
%%% Use Cases:
%%%   The network / network simulator will send usage events to the circuit.
%%%   The circuit records the usage of each service (for billing).
%%%   The circuit also aggregates demand to transformers.
%%% Circuit receives business events:
%%% - Meter (device) reports service usage.
%%% - Record service usage (usage history).
%%% - Meter (device) reports events (example: power off/on).
%%% - Meter reports time-series.
%%% 
%%% Implementation:
%%%   Each circuit is represented by a process (PID).
%%%   The network (simulator) maps circuits to PID and sends events to PID.
%%% This circuit_test only needs to send events to a single circuit PID.

-module(circuit_test).
-define( TEST, true ).
-define( DEBUG, true ).


%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

% A period of time defined by a start and end time.
-record( period, {udt_start, udt_stop }).

% A measurement of service usage.
-record( usage, { quantity, units, period } ).

% A generic time-series (intervals defined by te series_type)
-record( series, { series_type, period, interval, values }).

% A simple event with no associated data.
-record( event, { event_type, udt } ).


%%
%% API Functions
%%



%%
%% Local Functions
%%

%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%

%%% - Meter (device) reports service usage.
setup_test() ->
		{ ok, CircuitPID } = gen_server:start_link( 
													 { local, circuit }   % Give it a global name.
													 , circuit, []        % The module containing and arguments for init/1.
													 , []                 % options
                           ),
		put( circuit, CircuitPID ),
	  % ?debugVal( {setup_test, CircuitPID } ),
		
		ok.


meter_reports_service_usage_test() ->
		
		Meter = "circuit_test_Meter",
		Period = #period{ udt_start = {{2011,11,11},{00,00,00}}        
										,  udt_stop = {{2011,11,11},{23,59,59}} },
		
		Usage = #usage{ quantity = 123
									, units = watt_hour
									, period = Period
									} ,
		Service = "circuit_test_Service",
		
		Circuit = get(circuit),
		%?debugVal( {get_circuit, Circuit } ),
		Circuit ! { meter, Meter, reports_usage, Usage, on_service, Service},
		ok.

%%% - Record service usage (usage history).
% record_service_usage_test() -> ok.

%%% - Meter (device) reports events (example: power off/on).
meter_reports_events_test() -> 
		Meter = "circuit_test_Meter",
		Event = #event{ event_type=power_off, udt={ {2011,11,11},{23,59,59} } },
		Circuit = get(circuit),
		Circuit ! { meter, Meter, reports_event, Event },	
		ok.

%%% - Meter reports time-series.
meter_reports_time_series_test() -> 
		Meter = "circuit_test_Meter",
		Period = #period{ udt_start = {{2011,11,11},{00,00,00}}        
										,  udt_stop = {{2011,11,11},{24,00,00}} },
		SeriesRecord = #series{ series_type = kW, period = Period, interval=3600, 
											      values = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] 
													},
		Circuit = get(circuit),
    Circuit ! { meter, Meter, reports_time_series, SeriesRecord },
		ok.

write_series_to_file_test() ->
		Circuit = get(circuit),
		Circuit ! { write_series_to_file, "circuit_test_series.txt" },
		ok.

