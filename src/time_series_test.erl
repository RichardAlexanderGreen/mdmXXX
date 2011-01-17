%% Copyright: Richard Alexander Green 2011
%% Created: Jan 14, 2011
%% Description: Perform arithmetic on time-series data.
%% Use Cases:
%% - Add time-series T1 to time-series T2 giving time-series T3.
%% - Subtract time-series T1 from time-series T2 giving time-series T3.
%% Background:
%% ::: One needs to select a grain-size for the time-series aggregates.
%% -  1 minute  =  60 seconds
%% - 15 minutes = 900 seconds
%% -  1 hour    =                           60 minutes =   3,600 seconds
%% - 24 hours   = 1 day                = 1,440 minutes =  86,400 seconds
%% -  1 week   =  7 days =  168 hours = 10,800 minutes = 604,800 seconds
%% - 52 weeks = 364 days = 8736 hours = 561,600 minutes = 33,696,000 seconds
%% - 365.25 days = 8766 hours 
%% Approach:
%% - Use seconds since ___ as an index.
%% - The end-time of each interval will define the seconds since.
%% - Use orddict to represent the time-series.



-module(time_series_test).

%%
%% Include files
%%
-define( TEST, true ).
-define( DEBUG, true ).

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


%%
%% Exported Functions
%%
-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new1_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {12,00,00},
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		Quantity = 120,
		IntervalSize = 3600, % One hour
		TimeSeries = time_series:new( UDT_Start, UDT_Stop, IntervalSize, Quantity ),
		%?debugVal( orddict:size(TimeSeries) ),
		?assertEqual( 1, orddict:size(TimeSeries) ),
		%?debugVal( TimeSeries ),
		?assertEqual(  [{17635619,120.0}], TimeSeries ),
		TimeSeries.

new2_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {13,00,00},
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		Quantity = 120,
		IntervalSize = 3600, % One hour
		TimeSeries = time_series:new( UDT_Start, UDT_Stop, IntervalSize, Quantity ),
		%?debugVal( orddict:size(TimeSeries) ),
		?assertEqual( 2, orddict:size(TimeSeries) ),
		%?debugVal( TimeSeries ),
		?assertEqual(  [{17635619,60.0},{17635620,60.0}], TimeSeries ),
		TimeSeries.

new_from_list_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {00,00,00},
		TimeStop  = {24,00,00},
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		Values = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24],	
		IntervalSize = 3600, % One hour
		TimeSeries = time_series:new_from_list( UDT_Start, UDT_Stop, IntervalSize,  Values ),
		%?debugVal( orddict:size(TimeSeries) ),
		?assertEqual( 24, orddict:size(TimeSeries) ),
		%?debugVal( TimeSeries ),
		?assertEqual(  [{17635609,1},
              {17635610,2},
              {17635611,3},
              {17635612,4},
              {17635613,5},
              {17635614,6},
              {17635615,7},
              {17635616,8},
              {17635617,9},
              {17635618,10},
              {17635619,11},
              {17635620,12},
							{17635621,13},
							{17635622,14},
							{17635623,15},
							{17635624,16},
							{17635625,17},
							{17635626,18},
							{17635627,19},
							{17635628,20},
							{17635629,21},
							{17635630,22},
							{17635631,23},
							{17635632,24} ]
										, TimeSeries ),
		TimeSeries.


add_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {13,00,00},
		TimeStop2 = {14,00,00},	
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		UDT_Stop2= { DayStop, TimeStop2 },
		Quantity = 1200,
		IntervalSize = 3600, % One hour
		TimeSeries1 = time_series:new( UDT_Start, UDT_Stop, IntervalSize, Quantity ),
		TimeSeries2 = time_series:new( UDT_Start, UDT_Stop2,IntervalSize, Quantity ),
		TimeSeries3 = time_series:add( TimeSeries1, TimeSeries2 ),
		%?debugVal( TimeSeries3 ),
		?assertEqual( 3, orddict:size(TimeSeries3) ),
		?assertEqual( [{17635619,1.0e3},{17635620,1.0e3},{17635621,400.0}], TimeSeries3 ),
		TimeSeries3.

subtract_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {14,00,00},
		TimeStop2 = {13,00,00},	
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		UDT_Stop2= { DayStop, TimeStop2 },
		Quantity = 1200,
		IntervalSize = 3600, % One hour
		TimeSeries1 = time_series:new( UDT_Start, UDT_Stop, IntervalSize, Quantity ),
		TimeSeries2 = time_series:new( UDT_Start, UDT_Stop2,IntervalSize, 600 ),
		TimeSeries3 = time_series:subtract( TimeSeries1, TimeSeries2 ),
		%?debugVal( TimeSeries3 ),
		?assertEqual( 3, orddict:size(TimeSeries3) ),
		?assertEqual( [{17635619,100.0},{17635620,100.0},{17635621,400.0}], TimeSeries3 ),
		TimeSeries3.
		
		
