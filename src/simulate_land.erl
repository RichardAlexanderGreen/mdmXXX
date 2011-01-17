%% Author: admin
%% Created: Dec 15, 2010
%% Description: TODO: Add description to simulate_land
-module(simulate_land).
-define( DEGREES_PER_METER, (1/111319.0) ).


%%
%% Include files
%%
-define( NODEBUG, true ).
-define( NOTEST, true ).
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

layout_circuit( CircuitID, Lat, Long, _LineID, N_ServicesRemain ) when N_ServicesRemain < 1 ->
		{ CircuitID, Lat, Long };

layout_circuit( CircuitID, Lat, Long, LineID, N_ServicesRemain ) ->
		N_PerLine = lists:min( [32, N_ServicesRemain] ),                                              % 32 services per line
		layout_service( CircuitID, Lat, Long, LineID, N_PerLine ),
		LineSpacing = 30,                                         % 30 meters is about 100 feet
		% 32 * 30 meters = 960 meters (Keep each circuit about 1 km square.)
		layout_circuit( CircuitID, Lat + (LineSpacing * ?DEGREES_PER_METER), Long, LineID + 1, N_ServicesRemain - N_PerLine ). % Moving North.


layout_service( _CircuitID, _Lat, _Long, _LineID, N_RemainOnLine ) when N_RemainOnLine < 1 ->
		ok;
layout_service( CircuitID, Lat, Long, LineID, N_RemainOnLine ) ->
		%new_service( Lat, Long, CircuitID, LineID, N_RemainOnLine ),
		%_new_service( Lat, Long, CircuitID, LineID, N_RemainOnLine ),
		new_service( Lat, Long, CircuitID, LineID, N_RemainOnLine ),
		ServiceSpacing = 30,
		layout_service( CircuitID, Lat, Long + (ServiceSpacing * ?DEGREES_PER_METER), LineID, N_RemainOnLine - 1 ). % Moving East.

new_service( Lat, Long, CircuitID, LineID, N_RemainOnLine ) -> 
		?debugVal( { new_service, Lat, Long, CircuitID, LineID, N_RemainOnLine} ),
		
		ServiceID = { service, CircuitID, LineID, N_RemainOnLine },
		PremiseID = { premise, CircuitID, LineID, N_RemainOnLine },
		Location = { Lat, Long },
		W5 = w5( "new_service", "simulate_land"),
		global:send( assets,  { service, ServiceID, installed_at_premise, PremiseID, W5 } ),
		NameValueList = [ {circuit, CircuitID}, {location,Location} ] ,
		global:send( assets,  { asset, ServiceID, has_attributes, NameValueList , W5 } ),		
		ok.


-ifdef( CENTROID ).
update_circuit_centroid( CircuitID, Lat, Long ) ->
		% Get Circuit's centroid.
		Centroid = assets:ask( { what_is_value_of_attribute, centroid, for_asset, CircuitID } ),	
		?debugVal( Centroid ),
		
		case Centroid of
				{ ok, none } ->
						NewCentroid = { Lat, Long, 1 };
				
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
		North = (50 - random:uniform(100) ) * 1000 * ?DEGREES_PER_METER, %Place circuits randomly in a 100 kilometer square 
		East  = (50 - random:uniform(100) ) * 1000 * ?DEGREES_PER_METER,
		
		layout_circuit( CircuitID, North, East , 1, CircuitSize ),	
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
		'734-223-6687' = Password,
		%mnesia:clear_table( entity_attribute ),
		%mnesia:clear_table( entity_relationship ),	
		ok = generate_circuits( 10, 100 ).
