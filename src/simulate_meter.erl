%%% Copyright: Richard Alexander Green 2011
%%% Description : Simulate a meter in the context of simulate_network driver.
%%% 
%% Approach:
%% - The simulator sends a tick event to a group of processes that represent the meters on the network.
%% - When the meter recieves the tick event, 
%% - -  it calculates the amount of usage to be added to the register and recorded in the interval history.
%% - - The meter has a week-long load profile that drives the calculation.
%% - The meter may also report usage depending on how it is configured.
%% - - For example, 
%% - - - The meter might report incremental usage as it occurs.
%% - - - The meter might report the register value every day at the same time (Clock_Seconds midnight).
%% - - - The meter might report usage only on request.
%% - Various network load-leveling strategies may be added to the simulator if the tick event is small-grained.
%% - - If data is reported at the same time daily or on request,
%% - - - the meter could wait a random interval (watching time in the tick) before sending data.
%%% --------------------------------------------------------------
%%% Network (network) :: Communicate data to/from devices using c12.19 and c12.22 protocols.
%%% Ask network to send requests to meters
%%%   network ! { broadcast_request, R, to_device_group, G }
%%%   network ! { send_request, R, to_device, D }
%%% Receive data from meters /devices
%%%   network ! { device, D, reports_register_series, S }
%%%   network ! { device, D, reports_interval_value_series, S }
%%%   network ! { device, D, reports_events, E }
%%% Setup
%%%   network ! { device, D, is_in_group, G }
%%% where
%%%   request() -> { verb, [parameters] }
%%%   verb() -> return_registers 	| return_channels 	| return_events 	| open_switch 	| close_switch 	| reprogram
%%%   parameters() -> [ RegisterType ] 	| [ ChannelType ] 	| [ EventType ] 	| [ { ProgramParameter, ParameterValue } ]
%%% --------------------------------------------------------------
%% Use Cases: (copied from mdm_protocol -- Not all of these are implemented in this simulator)
%%% --------------------------------------------------------------
%%% Meter / Device (via Network)
%%% Data Acquistion Commands to Meter (Device)
%%%   Device ! { send_register_type, RegisterType, for_time_period, TimePeriod }
%%%   Device ! { send_inverval_channel, ChannelType, for_time_period, TimePeriod }
%%%   Device ! { send_event_data, EventType, for_time_period, TimePeriod }
%%% Device Control Commands
%%%   Device ! { open_switch }
%%%   Device ! { close_switch }
%%% Setup
%%%   Device ! { respond_to_group, G }
%%%   Device ! { ignore_group, G }
%%%   Device ! { capture_registers_daily_at, TimeOfDay }
%%%   Device ! { process_request, R, daily_between, TimeOfDay, and, TimeOfDay }
%%% Electric Meter State Elements 
%%%   (Switch Open/Closed, LoadSideVoltage/Not, Send Pending/Not, Power On/Off)
%%%   Switch Open -- No energy can flow.
%%%   Load Side Voltage -- Cannot close switch
%%%   Switch Closed -- Expect energy to flow.
%%%   Send Pending -- Requested/Scheduled data will be sent after a random delay
%%%   No Send Pending -- One less thing to do on a tick
%%%   Power Off -- A power outage is in-progress -- No energy can flow
%%%   Power On  -- If Power is On and Switch is closed, some energy usage may occur.
%%%   (This simulation does not include flow-control Ack/Nak behavior)
%%%   (This simulation does not include mesh-net or HAN behavior)
%%% --------------------------------------------------------------

-module(simulate_meter).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( TEST, true ).
-define( DEBUG, true ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).  % LOG info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% States
%-export( [ power_on/2, power_off/2 ] ).

%-record( state, {  }).
-record( state_data, { switch } ).


%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          	|
%%          {ok, StateName, StateData, Timeout} 	|
%%          ignore                              	|
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init( ArgsDict ) ->
    % ArgsDict is orddict with weekly_profile device_ID service_ID coordinates first_tick
		process_flag( trap_exit, true ),  % Enable my terminate when my parent terminates
		% Initialize with weekly-profile | device_ID | service_ID | latitude | longitude 
		store( weekly_profile, orddict:fetch( weekly_profile, ArgsDict )  ),
		store( device_ID,      orddict:fetch( device_ID,      ArgsDict )  ),
		store( service_ID,     orddict:fetch( service_ID,     ArgsDict )  ),
		store( latitude,       orddict:fetch( coordinates,    ArgsDict )  ),
		% Store empty interval history
		store( intervals, [] ),
		% Store zero register
		store( register, 0.0 ),
		% Prior tick
		store( prior_tick, orddict:fetch( first_tick, ArgsDict )  ),
		% Return {ok,StateName,StateData} -OR- {ok,StateName,StateData,Timeout}  
    {ok, power_on, #state_data{ switch = closed }}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          	|
%%          {next_state, NextStateName, NextStateData, Timeout} 	|
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
% state_name(Event, StateData) ->
%    {next_state, state_name, StateData}.
%% --------------------------------------------------------------------
% STATE-TRANSITION DECISION TABLE* 
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------
% 	| Service 	| Switch 	| Load Side | Power 	| Send    	||                                  	|| Service 	| Switch 	| Load  | Power 	| Send    	| Message 
% 	| Stopped 	| Position| Voltage		| On/Off 	| Pending   || Event                             	|| Stopped 	| Position| Volts | On/Off 	| Pending   | Queued 
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------
% 	| _       	| _      	| _       	| _     	| _       	|| Reset / Install ==> Normal State  	|| No      	| Closed 	| None 	 | On    	| Yes     	| Event: Installed at . . .
% 	| No      	| Closed 	| None    	| On    	| _       	|| Tick ==> Increment Usage          	|| ::::::::::::: No Change :::::::::::::::::::::::::  
% 	| _         | _      	| None      | On    	| Yes       || Tick ==> Send when time >= clock  	|| ::::::::::::: No Change :::::::::::::::::::::::::            	 
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------ 	  
% 	| No        | Closed 	| None      | On    	| _         || Stop service                      	|| Yes      | *Open*  | None   | On    	| _         | 
% 	| Yes       | Open   	| None      | On    	| _         || Resume service                    	|| *No*     | *Closed*| None   | On    	| _         | 
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------
% 	| _ 				| Closed 	| _ 	 			| Off 		| _ 				|| Load-Side Voltage Raised 					|| _ 				| *Open* 	| Raised | Off 		| *Yes* 		| Exception: Load-side voltage detected during power outage.
% 	| _ 				| Open 		| _ 				| _ 			| _ 				|| Load-Side Voltage Raised 					|| should?  | Open 		|*Raised*| _  		| _ 				| 
% 	| Yes 			| Open 		| Raised 		| On 			| _ 				|| Resume service 										|| *No* 		| Open 		| Raised | On 		| _ 				| 
% 	| No 				| Open 		| Raised 		| On 			| _ 				|| Load-Side Voltage Removed 					|| No 			| *Closed*| None   | On    	| _         | 
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------
% 	| _ 				| _ 			| _ 				| On 			| _ 				|| Power Out 													|| _ 				| _ 			| _ 		 | *Off* 	| _ 				| Event: (Last Gasp) Power Out at ...
% 	| _ 				| _ 			| _ 				| Off 		| _ 				|| Ignore all but Power Restored 			|| ::::::::::::: No Change ::::::::  (Meter radio has no power)
% 	| _ 				| _ 			| None 			| Off 		| _ 				|| Power Restored 										|| _ 				| _ 			| None 	 | *On* 	| _ 				| 
% 	| _ 				| _ 			| Raised 		| Off 		| _ 				|| Power Restored 										|| _ 				| *Open* 	| Raised | *On* 	| *Yes* 		| Exception: Cannot restore power because load-side voltage detected.
%		 ----------- --------- ----------- --------- -----------  ------------------------------------  ---------- --------- ------- --------- ----------- ------------------------------------
% * The tabs line-up in MY editor.
%
power_off( Event, StateData ) ->
		case { Event, load_side_voltage() } of
				{{ power_restored, _Clock_Seconds }, no_load_side_voltage } ->
						{ next_state, power_on, StateData };
				{{ power_restored, Clock_Seconds }, see_load_side_voltage } ->
						send( { exception, power_could_not_be_restored_due_to_load_side_voltage, mock_w5( Clock_Seconds ) } ),  % Someone will contact resident about this unsafe condition
						{ next_state, power_off, StateData };
				{ _IgnoreOtherEvents, _load_side_voltage_not_seen_yet } ->
						{ next_state, power_off, StateData }
		end.

power_on( {tick, Clock_Seconds}, StateData ) ->
		SwitchOpenClosed = StateData#state_data.switch,
		NewStateData = advance_usage( Clock_Seconds, SwitchOpenClosed ),		
		send_pending_message_when_due( Clock_Seconds ),
		{ next_state, power_on, NewStateData };		
		
power_on( { power_out, Clock_Seconds }, StateData ) ->
		send_last_gasp( { power_out_alarm, mock_w5( Clock_Seconds ) } ),   % Last-gasp send is powered by a capacitor (no battery to replace)
		{ next_state, power_off, StateData };

power_on( { stop_service, _Clock_Seconds }, StateData ) ->           % Open the switch  ( Collections / Emergency-Service / Load-Shedding requested )
		NewStateData = StateData#state_data{ switch = closed },
		{ next_state, power_on, NewStateData };

power_on( { resume_service, Clock_Seconds }, StateData ) ->    % Close the switch unless there is load-side voltage
		case load_side_voltage() of
				no_load_side_voltage -> 
						NewStateData = StateData#state_data{ switch = closed },  % Can safely close the switch
						{ next_state, power_on, NewStateData };
				see_load_side_voltage ->
						send( { exception, service_could_not_be_restored_due_to_load_side_voltage,mock_w5( Clock_Seconds ) }), % Someone will contact resident about this unsafe condition
						NewStateData = StateData#state_data{ switch = open },    % Switch stays open
						{ next_state, power_on, NewStateData }
		end;

power_on( { load_side_voltage_corrected, Clock_Seconds },  StateData ) ->
		send( { info, load_side_voltage_corrected, mock_w5( Clock_Seconds ) } ),
		NewStateData = StateData#state_data{ switch = closed },          % Can safely close the switch now
		{ next_state, power_on, NewStateData }.

% Provide data in data sent (especially exception reports) -- from my init() arguments
mock_w5( Clock_Seconds ) ->
		W5 = Clock_Seconds,
		Who = ?MODULE,
		Where = { my( device_ID ), my( service_ID ), my( latitude ), my( longitude ) },
		WhenUDT = calendar:gregorian_seconds_to_datetime( Clock_Seconds ),
		What = Why = 'simulation',
	  { w5, W5, Who, What, WhenUDT, Where, Why }.

% Get datum from repository / state 
% TODO -- SHOULD WE PUT This data in StateData instead?
my( Slot_Name ) ->
		get( Slot_Name ).  
store( Slot_Name, Value ) ->
		%?debugVal( { store, Slot_Name, Value } ),
		put( Slot_Name, Value ).

% Is there load-side voltage?
load_side_voltage() ->  % generate this condition at random if included in this meter`s profile
		case bad_generation_installation() of
				true -> 
						see_load_side_voltage;
				_ ->
						no_load_side_voltage
		end.

% Attempt a last-gasp message on power outage
send_last_gasp( Message ) ->     					% Send fails at random due to network collisions
		case network_collision() of
				true ->
						 do_nothing;
				_ ->
						send( Message )
		end.

-ifdef( TEST ).   
% MOCK THE send/1 function
send( Message ) ->
		dummy.
-else.
% Allow the send/1 to function.
% Send notice / exception / response (All of these go to data collector and are re-dispatched from there)
send( Message ) ->                         
		global:send( data_collector, Message ),  % TODO -- Simulate randomized send to spread message load.
		ok.
-endif.


% Simulate a badly installed generator or a long cord next door
bad_generation_installation() ->           % TODO -- This is a dummy. 
		% Is the random number in 0 to 1 range less than X ?
		false.

% Simulate a netwok collision (on a last-gasp notice)
network_collision() ->                     % TODO -- This is a dummy.
		% Is the random number in 0 to 1 range less than X ?
		false.

% Simulate energy usage according to the my profile.

advance_usage( Clock_Seconds, SwitchOpenClosed ) ->     % TODO -- Fix this to allow more than one tick per hour
		
		case SwitchOpenClosed of
				closed ->
						% Clock_Seconds is from calendar:datetime_to_gregorian_seconds(_)
						{ Date, Time } = calendar:gregorian_seconds_to_datetime( Clock_Seconds ),
						% Get day of week
						DayIndex = calendar:day_of_the_week( Date ) - 1 ,	
						% Get hour of day
						{ HourIndex, _Minute, _Second } = Time,
						% Look-up typical amount for that slot
						WeeklyProfile = my( weekly_profile ),				
						TypicalUsage = lists:nth( (DayIndex * 24) + (HourIndex + 1), WeeklyProfile ),
						% Randomize it a little bit
						Increment = TypicalUsage * ( 90.0 + random:uniform(20) ) / 100.0;
				open ->
						Increment = 0.0;
				_ ->
						Increment = 0.0,
						error_logger:error_report({logic_error, 'Switch should be either open/closed in ',?MODULE})
		end,
		% Add the increment to the usage (Wh) register 
		store(register, Increment + my( register) ),
		% Insert the increment in the interval history
		Intervals = orddict:update_counter( Clock_Seconds, Increment, my( intervals ) ) ,
		% And save it back
		store( intervals,  Intervals ),
		ok.

send_pending_message_when_due( _Clock_Seconds ) ->    % TODO -- This is a dummy.
		dummy.
	

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          	|
%%          {next_state, NextStateName, NextStateData, Timeout} 	|
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

handle_event( Event, StateName, StateData) ->
		error_logger:error_report( logic_error, ?MODULE,'handle_event/3 did_not_expect_event: ', Event ),
    { next_state, StateName, StateData }.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            	|
%%          {next_state, NextStateName, NextStateData, Timeout}   	|
%%          {reply, Reply, NextStateName, NextStateData}          	|
%%          {reply, Reply, NextStateName, NextStateData, Timeout} 	|
%%          {stop, Reason, NewStateData}                          	|
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event( Event, From, StateName, StateData ) ->
    error_logger:error_report( logic_error, ?MODULE,'handle_sync_event/4 did not expect event: ', Event,' From: ', From ),
		Reply = logic_error, 
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          	|
%%          {next_state, NextStateName, NextStateData, Timeout} 	|
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info( Message, StateName, StateData ) ->
    error_logger:error_report( logic_error, ?MODULE,'handle_info/3 did not expect message: ', Message ),
		{next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate( _Reason, _StateName, _StatData) ->
		% TODO -- Should we save simulated meter state on termination ???
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
		% TODO -- code_change/4 is not implemented -- Probably not needed
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

weekly_profile( Multiplier ) ->
		% Hour      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3
		Weekday = [ 0,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,2,2,1,1,1,0,0,0],
		Weekend = [ 0,0,0,0,0,0,1,1,1,1,1,1,2,1,1,1,2,2,1,1,1,0,0,0],
		DaysProfile   = Weekday % Monday is day 1
                 ++ Weekday % Tuesday
                 ++ Weekday % Wednesday
                 ++ Weekday % Thursday
                 ++ Weekday % Friday
                 ++ Weekend % Saturday
                 ++ Weekend,% Sunday
		% Add in some baseload ()
    WeeklyProfile = lists:map( fun(X) -> (X + 0.5) end, DaysProfile ).
                 
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weekly_profile_test() ->
		Multiplier = 1.0,	
		WeeklyProfile = weekly_profile( Multiplier ),
		WeeklyProfile.
	
init_test() ->
% ArgsDict is orddict with weekly_profile device_ID service_ID coordinates first_tick
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		ArgsDict = orddict:from_list( [ {weekly_profile, weekly_profile( 1.0 ) }
																	, {device_ID, 'simulate_meter device_ID'}
																	, {service_ID, 'simulate_meter service_ID'}
																	, {coordinates, {43.0, 83.0} }
																	, {first_tick, First_tick_clock_seconds}
																	]),
	  Result = init( ArgsDict ),
		%?debugVal( Result ),
		
		ok.

power_on_tick_test() ->
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600,
		StateData = #state_data{ switch = closed },
		Result = power_on( {tick, Clock_Seconds}, StateData ),
		%?debugVal( Result ),
		%?debugVal( my( intervals )  ),
		Result.

power_on_tick_tick_test() ->
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600 + 3600,
		StateData = #state_data{ switch = closed },
		Result = power_on( {tick, Clock_Seconds}, StateData ),
		%?debugVal( Result ),
		%?debugVal( my( intervals )  ),
		Result.

power_on_stop_service_test() ->
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600 + 3600,
		StateData = #state_data{ switch = closed },
		Result =  power_on( { stop_service, Clock_Seconds }, StateData ),
		{ next_state, power_on, NewStateData } = Result,
		?assertEqual( closed, NewStateData#state_data.switch ).

power_on_resume_service_test() ->
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600 + 3600,
		StateData = #state_data{ switch = open },
		Result =  power_on( { resume_service, Clock_Seconds }, StateData ),
		{ next_state, power_on, NewStateData } = Result,
		?assertEqual( closed, NewStateData#state_data.switch ).

power_on_load_side_voltage_corrected_test() ->
		%TODO - Need some way to cause load-side-voltage-effects in power_on and resume_service 
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600 + 3600 + 3600,
		StateData = #state_data{ switch = open },
		Result =  power_on( { load_side_voltage_corrected, Clock_Seconds }, StateData ),
		{ next_state, power_on, NewStateData } = Result,
		?assertEqual( closed, NewStateData#state_data.switch ).

power_off_power_restored_test() ->
		%TODO - Need some way to test load-side-voltage-effects
		Date = {2011,11,11},
		Time = {00,00,00},
		DateTime = { Date, Time },
		First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
		Clock_Seconds = First_tick_clock_seconds + 3600 + 3600 + 3600 + 3600,
		StateData = #state_data{ switch = closed },
		Result =  power_off( { power_restored, Clock_Seconds }, StateData ),
		{ next_state, power_on, NewStateData } = Result,
		?assertEqual( closed, NewStateData#state_data.switch ).