%% Copyright: Richard Alexander Green 2011
%% Created: Dec 15, 2010
%% Description: Create circuits with meters and lays them out with Lat-Long coordinates.
%% - You can also use this to run load tests on the assets actor (server) and persistence service (quad).


-module(simulate_land).
-define( DEGREES_PER_METER, (1/111319.0) ).


%%
%% Include files
%%
-define( NODEBUG, true ).
-define( TEST, true ).
-include_lib("eunit/include/eunit.hrl").

-ifdef( TEST ).
-compile( export_all ).
-endif.

-import( quad, [w5/2, w5/5] ).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

%---------------------------------------------------------------------------------
% Layout services in a circuit.

layout_circuit( CircuitID, Coordinates, _LineID, N_ServicesRemain ) when N_ServicesRemain < 1 ->
		{ CircuitID, Coordinates };

layout_circuit( CircuitID, Coordinates, LineID, N_ServicesRemain ) ->
		N_PerLine = lists:min( [32, N_ServicesRemain] ),                                              % 32 services per line
		layout_services( CircuitID, Coordinates, LineID, N_PerLine ),
		LineSpacing = 30,                                         % 30 meters is about 100 feet
		% 32 * 30 meters = 960 meters (Keep each circuit about 1 km square.)
		{Lat, Long } = Coordinates,
		NewCoordinates = { Lat + (LineSpacing * ?DEGREES_PER_METER), Long }, 
		layout_circuit( CircuitID, NewCoordinates , LineID + 1, N_ServicesRemain - N_PerLine ). % Moving North.


layout_services( _CircuitID, _Coordinates, _LineID, N_RemainOnLine ) when N_RemainOnLine < 1 ->
		ok;
layout_services( CircuitID, Coordinates, LineID, N_RemainOnLine ) ->
		%new_service( Coordinates, CircuitID, LineID, N_RemainOnLine ),
		%_new_service( Coordinates, CircuitID, LineID, N_RemainOnLine ),
		new_service( Coordinates, CircuitID, LineID, N_RemainOnLine ),
		ServiceSpacing = 30, % meters ~ 100 feet
		{Lat, Long } = Coordinates,
		NewCoordinates = { Lat, Long + (ServiceSpacing * ?DEGREES_PER_METER) },
		layout_services( CircuitID, NewCoordinates, LineID, N_RemainOnLine - 1 ). % Moving East.

new_service( Coordinates, CircuitID, LineID, N_RemainOnLine ) -> 
		?debugVal( { new_service, Coordinates, CircuitID, LineID, N_RemainOnLine } ),
		
		ServiceID = { service, CircuitID, LineID, N_RemainOnLine },
		PremiseID = { premise, CircuitID, LineID, N_RemainOnLine },
		W5 = w5( "new_service", "simulate_land"),
		global:send( assets,  { service, ServiceID, installed_at_premise, PremiseID, W5 } ),
		NameValueList = [ {circuit, CircuitID}, {location, Coordinates} ] ,
		tell( assets, { asset, ServiceID, has_attributes, NameValueList , W5 } ),		
		tell( assets, { service, ServiceID, installed_on_circuit, CircuitID, W5 }),
		ok.

tell( Actor, Message ) ->
		global:send( Actor, Message ),
		Log = get(log),
		case Log of
				undefined ->
					do_nothing;
				_ ->
					disk_log:log( Log, {Actor, Message } )
		end.


-ifdef( CENTROID ).
update_circuit_centroid( CircuitID, Lat, Long ) ->
		% Get Circuit's centroid.
		Centroid = assets:ask( { what_is_value_of_attribute, centroid, for_asset, CircuitID } ),	
		?debugVal( Centroid ),
		
		case Centroid of
				{ ok, none } ->
						NewCentroid = { Coordinates, 1 };
				
				{ok, { CentroidLat, CentroidLong, N_Points } } ->
					  NewPoints = N_Points + 1,
						NewCentroidLat = (( CentroidLat * N_Points ) + Lat ) / NewPoints,
						NewCentroidLong = (( CentroidLong * N_Points ) + Long ) / NewPoints,
						NewCentroid = { NewCentroidLat, NewCentroidLong, NewPoints }
				
				%_Otherwise -> exit( {fell_thru_case_in_update_circuit_centroid, did_not_match, Centroid } )
				end,		
		global:send( assets,  { asset, CircuitID, has_attribute, centroid, value, NewCentroid, now() },
		NewCentroid.
-endif.


%---------------------------------------------------------------------------------
% Generate a bunch of circuits.
% generate_circuits( N_Circuits, ListOfCircuits )
% - N_Circuits :: number of new circuits to generate
% - ListOfCircuits :: an empty list or an existing list of CircuitIDs

generate_circuits( N_Circuits, _CircuitSize ) when N_Circuits < 1 ->
		ok;

generate_circuits( N_Circuits, CircuitSize ) ->
		CircuitID = N_Circuits,		
		Latitude   = (50 - random:uniform(100) ) * 1000 * ?DEGREES_PER_METER, %Place circuits randomly in a 100 kilometer square 
		Longitude  = (50 - random:uniform(100) ) * 1000 * ?DEGREES_PER_METER,
		Coordinates = { Latitude, Longitude },
		layout_circuit( CircuitID, Coordinates, 1, CircuitSize ),	
		generate_circuits( N_Circuits - 1 , CircuitSize ).

%---------------------------------------------------------------------------------
%% ===============================================================================
%% Tests


-ifdef( CENTROID ).
update_circuit_centroid_test( ) ->
  Result = update_circuit_centroid( 'Test CircuitID', 43.00000, 83.00000 ),
	?assertMatch( {43.0,83.0,_},  Result ).
-endif.
	
generate_circuits_test() ->
		%mnesia:clear_table( entity_attribute ),
		%mnesia:clear_table( entity_relationship ),	
		ok = generate_circuits( 2, 5 ).

generate_circuits_load_test( Password ) ->
		'GrossDerelictionOfDuty' = Password,
		%mnesia:clear_table( entity_attribute ),
		%mnesia:clear_table( entity_relationship ),	
		ok = generate_circuits( 10, 100 ).
