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



-module(time_series).

%%
%% Include files
%%
-define( NOTEST, true ).
-define( NODEBUG, true ).

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


%%
%% Exported Functions
%%
-export([add/2, subtract/2, new/4]).

%%
%% API Functions
%%
% -------------------------------------------------------------------------------------------------
%% - Add time-series T1 to time-series T2 giving time-series T3.
add( T1, T2 ) ->
		%Fun = fun(Key, Value1, Value2) -> Value1 + Value2 end,				 
		%T3 = ordict:merge( Fun, T1, T2 ),
		T3 = orddict:merge( fun(_Key, Value1, Value2) -> (Value1 + Value2) end, T1, T2 ),
		T3.
% -------------------------------------------------------------------------------------------------
%% - Sbutract time-series T2 from time-series T1 giving time-series T3.
subtract( T1, T2 ) ->
		Fun = fun(Key, Value1, Value2) -> Value1 - Value2 end,				 
		T3 = orddict:merge( Fun, T1, T2 ),
		T3.
% -------------------------------------------------------------------------------------------------
%% - Create an empty time-series.
new() ->
		TimeSeries = orddict:new(),
		TimeSeries.
% -------------------------------------------------------------------------------------------------

%% - Create a new time-series by spreading quantity over itervals.
new( UDT_Start, UDT_Stop, IntervalSize, Quantity ) ->
		% Require that UDT times are on interval boundaries.
		check( UDT_Start,  IntervalSize ),
		check( UDT_Stop, IntervalSize ),
		% This decomposition may be redundant -- but it makes what we are doing transparent
		{DateStart, TimeStart} = UDT_Start,
		IndexStart = calendar:datetime_to_gregorian_seconds({DateStart, TimeStart}) div IntervalSize,
		{DateStop, TimeStop} = UDT_Stop,
		IndexStop = calendar:datetime_to_gregorian_seconds({DateStop, TimeStop}) div IntervalSize,
		TimeSeries0 = new(),
		N_Intervals = IndexStop - IndexStart,
		QuantityPerInterval = Quantity / N_Intervals,	
		TimeSeriesResult = putIntervals(  QuantityPerInterval, IndexStart, IndexStop, TimeSeries0 ),
		TimeSeriesResult.
putIntervals( QtyPer, IndexStart, IndexStop, TimeSeries )  when IndexStart == IndexStop   ->  % The indexes are equal - No more interation
		TimeSeries;

putIntervals( _QtyPer, IndexStart, IndexStop, _TimeSeriesIn ) when IndexStart > IndexStop ->
		?debugVal( { logic_error, ?MODULE, putInterval, IndexStart, IndexStop } ),
		error_logger:error_report({logic_error, ?MODULE, putInterval, IndexStart, IndexStop }),
		'FAIL - TROUBLE IN putIntevals';

putIntervals( QtyPer, IndexStart, IndexStop, TimeSeriesIn ) ->
		%?debugVal( { trace, putInterval, QtyPer, IndexStart, IndexStop } ),
		TimeSeriesOut = orddict:store( IndexStart, QtyPer, TimeSeriesIn ),
		putIntervals( QtyPer, IndexStart + 1, IndexStop, TimeSeriesOut ).
		
check( UDT_Time,  IntervalSize ) ->
		{ Date, Time } = UDT_Time,  % FAIL if UDT_Time is not in expected format
		%?debugVal( calendar:datetime_to_gregorian_seconds({Date, Time}) rem IntervalSize ),
		
		0 = calendar:datetime_to_gregorian_seconds({Date, Time}) rem IntervalSize.  % FAIL if given time is not on interval boundary.

% -------------------------------------------------------------------------------------------------
% Create a new time-series by from a list of values
new_from_list( UDT_Start, UDT_Stop, IntervalSize, ValueList ) ->
		TimeList = make_times( UDT_Start, UDT_Stop, IntervalSize ),
		TimeSeries = lists:zip( TimeList, ValueList ),  % [{index,value}]
		TimeSeries.

make_times( UDT_Start, UDT_Stop, IntervalSize ) ->
		{DateStart, TimeStart} = UDT_Start,
		IndexStart = calendar:datetime_to_gregorian_seconds({DateStart, TimeStart}) div IntervalSize,
		{DateStop, TimeStop} = UDT_Stop,
		IndexStop = calendar:datetime_to_gregorian_seconds({DateStop, TimeStop}) div IntervalSize,
		make_time_list( IndexStart, IndexStop, [] ).

make_time_list( IndexStart, IndexStop, TimeList ) when IndexStart == IndexStop ->
		TimeList;
make_time_list( IndexStart, IndexStop, TimeList ) when IndexStart > IndexStop ->
		?debugVal( { logic_error, ?MODULE, make_time_list, IndexStart, IndexStop } ),
		error_logger:error_report({logic_error, ?MODULE, make_time_list, IndexStart, IndexStop }),
		'FAIL - TROUBLE IN make_time_list';
make_time_list( IndexStart, IndexStop, TimeList ) when IndexStart < IndexStop ->
		NewTimeList = [IndexStop] ++ TimeList,
		make_time_list( IndexStart, IndexStop-1, NewTimeList ).
% -------------------------------------------------------------------------------------------------


	
%%
%% Local Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_test() ->
		TimeSeries = new(),
		?assertEqual( 0, orddict:size( TimesSeries ) ).
		

new4a_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {12,00,00},
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		Quantity = 120,
		IntervalSize = 3600, % One hour
		TimeSeries = new( UDT_Start, UDT_Stop,  IntervalSize, Quantity ),
		%?debugVal( orddict:size(TimeSeries) ),
		?assertEqual( 1, orddict:size(TimeSeries) ),
		%?debugVal( TimeSeries ),
		?assertEqual(  [{17635619,120.0}], TimeSeries ),
		TimeSeries.

new4b_test() ->
		DayStart = {2011,11,11},
		DayStop = DayStart,
		TimeStart = {11,00,00},
		TimeStop  = {13,00,00},
		UDT_Start = { DayStart, TimeStart },
		UDT_Stop = { DayStop, TimeStop },
		Quantity = 120,
		IntervalSize = 3600, % One hour
		TimeSeries = new( UDT_Start, UDT_Stop, IntervalSize,  Quantity ),
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
		TimeSeries = new_from_list( UDT_Start, UDT_Stop, IntervalSize,  Values ),
		%?debugVal( orddict:size(TimeSeries) ),
		?assertEqual( 2, orddict:size(TimeSeries) ),
		%?debugVal( TimeSeries ),
		?assertEqual(  [{17635619,60.0},{17635620,60.0}], TimeSeries ),
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
		TimeSeries1 = new( UDT_Start, UDT_Stop, Quantity, IntervalSize ),
		TimeSeries2 = new( UDT_Start, UDT_Stop2, Quantity, IntervalSize ),
		TimeSeries3 = add( TimeSeries1, TimeSeries2 ),
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
		TimeSeries1 = new( UDT_Start, UDT_Stop, Quantity, IntervalSize ),
		TimeSeries2 = new( UDT_Start, UDT_Stop2, 600, IntervalSize ),
		TimeSeries3 = subtract( TimeSeries1, TimeSeries2 ),
		%?debugVal( TimeSeries3 ),
		?assertEqual( 3, orddict:size(TimeSeries3) ),
		?assertEqual( [{17635619,100.0},{17635620,100.0},{17635621,400.0}], TimeSeries3 ),
		TimeSeries3.
		
		
