%%% -------------------------------------------------------------------
%%% Copyright : Richard Alexander Green  -- 2010
%%% Description : Each instance monitors usage on a circuit.
%%%  Circuit is used with electricity distribution in mind.
%%%  However, the concept could be applied to the distribution 
%%%    of gas or water.
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
%%% -------------------------------------------------------------------
-module(circuit).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( TEST, true ).
-define( NODEBUG, true ).
-include_lib("eunit/include/eunit.hrl").

% Reports below the current LOGLEVEL are ignoared.
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
-define( LOGLEVEL, 3 ).

-include( "actor.hrl" ).  %% Include gen_server skeleton.
%% I must define do/2, answer/3, custom_init/1, custom_shutdown/1

%% --------------------------------------------------------------------

% A period of time defined by a start and end time.
-record( period, {udt_start, udt_stop }).

% A measurement of service usage.
-record( usage, { quantity, units, period } ).

% A generic time-series with intervals defined by the series_type, interval-size (in seconds) 
-record( series, { series_type, period, interval, values }).

% A simple event with no associated data.
-record( event, { event_type, udt } ).


%% ------------------------------------------------------------------------------
%% do( Action, State ) -> State2.

do( { meter, Meter, reports_usage, Usage, on_service, Service}, State ) ->
		%?debugVal( { meter, Meter, reports_usage, Usage, on_service, Service} ),
		% Add usage to circuit usage.
		% Forward usage to usage actor -- Record service usage history.
		State;

do( { meter, Meter, reports_event, Event } , State ) ->
		?debugVal( { meter, Meter, reports_event, Event }),
		% Forward event to events actor -- Log events from all meters.
		State;

do( { meter, Meter, reports_time_series, SeriesRecord }, State ) ->
		?debugVal( { meter, Meter, reports_time_series, SeriesRecord } ),
		UDT_Start = SeriesRecord#series.period#period.udt_start,
		UDT_Stop  = SeriesRecord#series.period#period.udt_stop,
		IntervalSize = SeriesRecord#series.interval,
		% TODO - check that the interval size matches my set-up (from ArgList)
		List = SeriesRecord#series.values,
		TimeSeriesIn = time_series:new_from_list( UDT_Start, UDT_Stop, IntervalSize, List ),
		
		% Add time-series to this circuit's time-series.
		% TODO: Assumes our caller will not send duplicate data.
		OldTimeSeries = get( time_series ),
		NewTimeSeries = time_series:add( OldTimeSeries, TimeSeriesIn ),
		put( time_series, NewTimeSeries ),                 % Is okay as long as circuit is called serially
		% Forward time-series to usage actor -- Record service usage history.
		State;

do( { write_series_to_file, File }, State ) ->
		{ ok, IO_Device } = file:open( File, [write] ),
		ok = file:write( IO_Device, io_lib:format("~w,~n", [ get( time_series ) ]  )   ),
		ok = file:close( IO_Device ),
		State;
		
do( Action, State ) ->
		error_logger:error_report(error, {?MODULE,'does not understand', Action}),
		log( error, {?MODULE,'does not understand action:', Action} ),
		State.

answer( Request, _From, _State ) ->
		?debugVal( {circuit_does_not_understand, Request } ),
		log( error, { ?MODULE,'does not understand request:',  Request, " from:", _From  } ),
		_State.

custom_init( ArgList )->
		?debugVal( {trace, custom_init} ),
		TimeSeries = time_series:new(),   % Create an empty time-series.
		put( time_series, TimeSeries ),   % Store it in my properties
		State = #state{},
		State.

custom_shutdown( State ) ->
		?debugVal( {trace, custom_shutdown}),		
		ok.

