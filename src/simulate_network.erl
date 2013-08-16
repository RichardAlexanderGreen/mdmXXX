%% Copyright: Richard Alexander Green 2011
%% Created: Jan 18, 2011
%% Description: Simulate a network of meters.
%% Approach:
%% - The simulator sends a tick event to a group of processes that represent the meters on the network.
%% - When the meter recieves the tick event, 
%% - -  it calculates the amount of usage to be added to the register and recorded in the interval history.
%% - - The meter has a week-long load profile that drives the calculation.
% ---------------------------------------------------------------------
% Simulator's state model:
% state * { message }       >> next state
% ---------------------------------------------------------------------
% idle * { initialize }     >> meters ready ( meter processes started )
% meters ready  * { start } >> clock running
% clock running * { tick }  >> clock running
% clock running * { stop }  >> meters ready
% meters ready  * { quit }  >> idle ( meter processes terminated )
% ---------------------------------------------------------------------
-module(simulate_network).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
-export([init/1,  handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record( state, { meter_list
								, prior_clock_seconds
								, stop_clock_after_seconds
								, milliseconds_per_tick
								, simulated_seconds_per_tick 
								, timer_ref
								}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, state_name, #state{}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
%state_name(Event, StateData) ->
%    {next_state, state_name, StateData}.
% ---------------------------------------------------------------------
% Simulator's state model:
% state * { message }       >> next state
% ---------------------------------------------------------------------
% idle * { initialize }     >> meters ready ( meter processes started )
% meters ready  * { start } >> clock running
% clock running * { tick }  >> clock running
% clock running * { stop }  >> clock stopped
% clock stopped * { quit }  >> idle ( meter processes terminated )
% ---------------------------------------------------------------------
idle( { initialize, CircuitID }, StateData ) ->
		% Set up the simulated meter processes
		MeterPIDs = start_meters( CircuitID ),
		NewStateData = StateData#state{ meter_list = MeterPIDs },
		{ next_state, meters_ready, NewStateData }.

meters_ready( { start }, StateData ) ->
		MillisecondsPerTick = StateData#state.milliseconds_per_tick,
		NewStateData = start_clock( MillisecondsPerTick, StateData ),
		{ next_state, clock_running, NewStateData }.

clock_running( { central_tick }, StateData ) ->
		% TODO -- Send tick with simulated UDT to all meters.
		ClockSeconds = StateData#state.prior_clock_seconds
                 + StateData#state.simulated_seconds_per_tick,
		if ( ClockSeconds > StateData#state.stop_clock_after_seconds ) ->
					 % Stop the clock
					 NewStateData = stop_clock( StateData ),
					 { next_state, clock_stopped, NewStateData};
			 true ->
					 % Keep on ticking
					 send_all_meters( { tick, ClockSeconds }, StateData ),
					 NewStateData = StateData#state{ prior_clock_seconds = ClockSeconds },
					 { next_state, clock_running, NewStateData }
		end;

clock_running( { stop }, StateData ) ->
		% TODO -- Stop the clock.
		NewStateData = stop_clock( StateData ),
		{ next_state, clock_stopped, NewStateData }.

clock_stopped( { quit }, StateData ) ->
		% TODO -- Terminate all the meter-PIDs 
		%--- (maybe we simply terminate ourself and they will go too)
		{ next_state, idle, StateData }.


%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%   {next_state, NextStateName, NextStateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
		error_logger:error_report( logic_error, ?MODULE,'handle_event/3 did_not_expect_event: ', Event ),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
     error_logger:error_report( logic_error, ?MODULE,'handle_sync_event/4 did not expect event: ', Event,' From: ', From ),
     Reply = logic_error,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info( Message, StateName, StateData) ->
    error_logger:error_report( logic_error, ?MODULE,'handle_info/3 did not expect message: ', Message ),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
		% TODO -- What should we write when simulate_network is terminated?
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
		% code_change/4 not implemented. Not needed.
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
% Set a PID going for each service in a circuit.
start_meters( CircuitID ) ->
		% Scan services produced by simulate_land and start a simulate_meter process for each one.
		ListOfServices = assets:list_all_services_on_circuit( CircuitID ),
		Fun = fun( ServiceID ) ->
									Name = ServiceID,
									TBD = much_more_work_to_do,         % TODO - WEEKLY PROFILE AND COORDINATES
									Date = {2011,11,11},
									Time = {00,00,00},
									DateTime = { Date, Time },
									First_tick_clock_seconds = calendar:datetime_to_gregorian_seconds( DateTime ),
									ArgsDict = orddict:from_list( [ {weekly_profile, simulate_meter:weekly_profile( 1.0 ) }
																	, {device_ID, "Device-" ++ ServiceID }
																	, {service_ID, ServiceID }
																	, {coordinates, {43.0, 83.0} }
																	, {first_tick, First_tick_clock_seconds}
																	]),				
									Options = [],
									{ ok, ProcessID } = gen_fsm:start_link({local, Name}, simulate_meter, ArgsDict, Options),
									{ ServiceID, ProcessID }
					end,
		lists:map( Fun, ListOfServices ).
		
% Send tick messages at some rate -- Each tick represents the passage of X seconds in time.
% Uses timer:send_interval( Time, Message ) to send tick to self
% Uses timer:send_interval( Time, Pid, Message ) uses to send tick with calculated UDT to meters.

start_clock( MillisecondsPerTick, StateData ) ->	
		 TimerRef = timer:send_interval( MillisecondsPerTick, { central_tick } ),
		 NewStateData = StateData#state{ timer_ref = TimerRef },
     NewStateData.

stop_clock( StateData ) ->
		{ok, cancel} = timer:cancel( StateData#state.timer_ref ),
		NewStateData = StateData#state{ timer_ref = timer_cancelled } ,
		NewStateData.

send_all_meters( Message, StateData ) ->
		MeterPIDs = StateData#state.meter_list,
		send_list( MeterPIDs, Message ).

send_list( [], _Message ) ->
		 done;
send_list( MeterPIDs, Message ) ->
		[ MeterPID | Remainder ] = MeterPIDs,	
		gen_fsm:send_event( MeterPID , Message ),
		send_list( Remainder, Message ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_meters_test() ->
		% Keep it minimal and set up one circuit with 3 meters.
		CircuitID = 'circuit ID for simulate_network test',
		Coordinates = { 43.0, 83.0 },
		LineID = 'line ID for simulate_network test',
		N_Services = 3,
		simulate_land:layout_circuit(CircuitID, Coordinates, LineID, N_Services),
		start_meters( CircuitID ).

		
