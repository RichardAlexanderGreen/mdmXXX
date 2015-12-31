%%% Meter Data Management Protocol
%%% Copyright: Richard Alexander Green 2011
%%% @doc
%%% Actors:
%%% -- asset_manager (assets) :: Track services and metering network devices.
%%% -- usage_history (usage) :: Record service usage measurements.
%%% -- event_history (events) :: Record service/device event history.
%%% -- field_service (field) :: Perform physical field work -- installing, removing, exchanging services and devices.
%%% -- installation_manager (installer) :: Track Mass Replacement Work Jobs.
%%% -- pricing (pricing) :: Price usage in real time (and send to billing).
%%% -- meter_network (network) :: Communicate data to/from meters using c12.19 and c12.22 protocols.
%%% -- dashboard (dashboard) :: Provide a real-time view of what is going on. 
%%% @end
%%% --------------------------------------------------------------
%%% Asset Manager (assets) :: Track services and metering network devices.
%%%   assets ! { service, S, usage_monitored_by_device, D, starting_at_time, T }
%%%   assets ! { service, S, installed_at_location, L }
%%%	  assets ! { service, S, removed }
%%%	  assets ! { asset, A, has_attributes, Dict } 
%%%	  assets ! { asset_class, C, has_attributes, Dict }
%%%	  assets:get( {asset, A, attributes } ) -> orddict().
%%% where
%%%	  time() -> { date(), time() }
%%%	  location() -> { latitude, longitude } -- GPS coordinates
%%%	  service() and device() are asset identifiers (inventory serial numbers)
%%%	  Dict is maintained in a list via orddict.
%%% --------------------------------------------------------------
%%% Usage History (usage) :: Record service usage measurements.
%%% 	usage ! { device, D, reports_usage, U, on_service, S }
%%%	  usage:ask( { usage_of_service, S, over_period, P } ) -> measurement().
%%% where
%%% 	usage() -> { qty(), units(), period() }
%%%	  period() -> { timestamp(), timestamp() }
%%%   timestamp() -> { date(), time() }
%%% --------------------------------------------------------------
%%% Event History (events) :: Record service/device event history.
%%%	  events ! { device, D, reports_event, E, on_service, S }
%%% where
%%% 	event() -> { event_type(), event_time(), details() } 
%%% --------------------------------------------------------------
%%% Field Service (field) :: Perform physical field work -- installing, removing, exchanging services and devices.
%%% 	field ! { install_service_type, T, at_address, A }
%%% 	field ! { install_device_type, T, on_service, S }
%%% 	field ! { replace_device, D, on_service, S, with_a_device_of_type, T }
%%% 	field ! { remove_device, D, from_service, S }
%%% 	field:get( { what_jobs_are_in_progress } ) -> [ job ].
%%% --------------------------------------------------------------
%%% Pricing :: Price usage in real time (and send to billing)
%%%	  pricing ! { service, S, is_priced_by_mechanism, M }
%%% 	pricing ! { service, S, had_usage, U }
%%% where
%%%	  mechanism() -> { module, function( usage() ) }
%%% --------------------------------------------------------------
%%% Installation Manager (installer) :: Track mass replacement work jobs.
%%% 	installer ! { schedule_group, G, with_services, [ Services ], for_period, P }
%%% where
%%%	  group() -> string()  % unique name for a set of services.
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
%%%   verb() -> return_registers | return_channels | return_events | open_switch | close_switch | reprogram
%%%   parameters() -> [ RegisterType ] | [ ChannelType ] | [ EventType ] | [ { ProgramParameter, ParameterValue } ]
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
%%%   
%%% --------------------------------------------------------------

-module( mdm_protocol).

-compile( [export_all] ).


%%% --------------------------------------------------------------
%%% Asset Manager (assets) :: Track services and metering network devices.
%%%   assets ! { service, S, usage_monitored_by_device, D, starting_at_time, T }
%%%   assets ! { service, S, installed_at_location, L }
%%%	  assets ! { service, S, removed }
%%%	  assets ! { asset, A, has_attributes, Dict } 
%%%	  assets ! { asset_class, C, has_attributes, Dict }
%%%	  assets:get( {asset, A, attributes } ) -> orddict().
%%% where
%%%	  time() -> { date(), time() }
%%%	  location() -> { latitude, longitude } -- GPS coordinates
%%%	  service() and device() are asset identifiers (inventory serial numbers)
%%%	  Dict is maintained in a list via orddict.
%%% --------------------------------------------------------------
asset_manager_test() ->
  % Assume assets (actor PID) is registered locally -- otherwise ! operator will not work.
	true = lists:member( assets, registered() ),
	
	% Install a service	
	Service = "Test-Service-ID",
	Location = { 43.000001, -83.000001 },   % Latitude, Longitude
 	assets ! { service, Service, installed_at_location, Location },

	% Install a meter on the service
	Device = "Test-Device-ID",
	Time = now(),
	assets ! { service, Service, usage_monitored_by_device, Device, starting_at_time, Time },

	% Remove the service
	assets ! { service, Service, removed },
	
	
	% Record asset class attributes.
	Class = 'AMI Mass Market Electric Meter',
	ClassDict = orddict:from_list( [ { asset_class,   Class },
																	 { configuration, 'Residential Service' },   % Default configuration - installed at factory.
																	 { manufacturer,  'Tarheel Electronics' }             % Primary manufacturer
																	 ] ),
  assets ! { asset_class, Class, has_attributes, ClassDict },
	
	% Record asset attributes.
	Asset = Device,
	Dict0 = ClassDict,  % TODO - Should retrieve from data store.
	Dict1 = orddict:store( asset_type, 'AMI Electric Meter', Dict0 ),
	Dict2 = orddict:store( configuration, 'Commercial Service', Dict1 ),   % Over-ride the default attribute.
	assets ! { asset, Asset, has_attributes, Dict2 },

	% Retrieve the asset attributes.
	ActualDict = assets:get( {asset, Asset, attributes } ),
	ActualDict.

%%% --------------------------------------------------------------
%%% Usage History (usage) :: Record service usage measurements.
%%% 	usage ! { device, D, reports_usage, U, on_service, S }
%%%	  usage:ask( { usage_of_service, S, over_period, P } ) -> measurement().
%%% where
%%% 	usage() -> { qty(), units(), begin_time(), end_time() }
%%%	  period() -> { begin_time(), end_time() }
%%% --------------------------------------------------------------

usage_history_test() ->
		% Assume usage is registered.
		true = lists:member( usage, registered() ),
	
	  % Send usage notice to Usage History.
		Device = "Test-Device-ID",
	  Service = "Test-Service-ID",
		Quantity = 1234,
		T1 = { date(), time() },
		T2 = { date(), time() },
		Period = { T1, T2 },
		Usage = { Quantity, watts, Period },
		usage ! { device, Device, reports_usage, Usage, on_service, Service },
		
		% Retrieve usage ( easy case first -- no sum required )
		{ Quantity, watts, Period } = usage:ask( { usage_of_service, Service, over_period, Period } ).

%%% --------------------------------------------------------------
%%% Event History (events) :: Record service/device event history.
%%%	  events ! { device, D, reports_event, E, on_service, S }
%%% where
%%% 	event() -> { event_type(), timestamp(), details() } 
%%% --------------------------------------------------------------

event_history_test() ->
		% Assume events is registered.
		true = lists:member( events, registered() ),
		
		% Send event to Event History.
		Device = "Test-Device-ID",
	  Service = "Test-Service-ID",
		Time = { date(), time() },
		Event = { power_down, Time, "All Phases" },
		events ! { device, Device, reports_event, Event, on_service, Service }.

%%% --------------------------------------------------------------
%%% Field Service (field) :: Perform physical field work -- installing, removing, exchanging services and devices.
%%% 	field ! { install_service_type, T, at_address, A }
%%% 	field ! { install_device_type, T, on_service, S }
%%% 	field ! { replace_device, D, on_service, S, with_a_device_of_type, T }
%%% 	field ! { remove_device, D, from_service, S }
%%% 	field:get( { what_jobs_are_in_progress } ) -> [ job ].
%%% --------------------------------------------------------------

field_service_test() ->
		% Assume field is registered.
		true = lists:member( field, registered() ),
		
		% Install a service.
		ServiceType = 'Standard Residential Electric',
		Address = { "2011 East Bogus Boulevard", 10099-1234 },
		field ! { install_service_type, ServiceType, at_address, Address },  % TODO - Should this be a call that returns a service-ID ?
		
		% Install a device (meter) on a service.
		DeviceType = "Standard Residential Electric Meter",
	  Service = "Test-Service-ID",
		field ! { install_device_type, DeviceType, on_service, Service },
		
		% Replace a device (meter).
		Device = "Test-Device-ID",
	  field ! { replace_device, Device, on_service, Service, with_a_device_of_type, DeviceType },
		
		% Remove a device (meter).
		field ! { remove_device, Device, from_service, Service },
		
		% Get work-in-progress (a list contain job data reports).
		JobList = field:get( { what_jobs_are_in_progress } ),
		JobList.

%%% --------------------------------------------------------------
%%% Pricing :: Price usage in real time (and send to billing)
%%%	  pricing ! { service, S, is_priced_by_mechanism, M }
%%%   pricing ! { service, S, had_usage, U }
%%% where
%%%	  mechanism() -> { module, function( usage() ) }
%%% --------------------------------------------------------------

pricing_test() ->
		% Assume pricing is registered.
		true = lists:member( pricing, registered() ),
		
		% Assign a price-mechanism (rate) to a service.
		Service = "Test-Service-ID",
		Mechanism = { module, function },
		pricing ! { service, Service, is_priced_by_mechanism, Mechanism },
		
		% Price some usage.
		Quantity = 1234,
		T1 = { date(), time() },
		T2 = { date(), time() },
		Period = { T1, T2 },
		Usage = { Quantity, watts, Period },
		pricing ! { service, Service, had_usage, Usage }. % TODO -- This seems a little too mysterious -- Where does the pricing result go?

%%% --------------------------------------------------------------
%%% Installation Manager (installer) :: Track Mass Replacement Work Jobs.
%%% 	installer ! { schedule_group, G, with_services, [ Services ], for_period, P }
%%% where
%%%	  group() -> string()  % unique name for a set of services.
%%% --------------------------------------------------------------

installer_test() ->
		% Assume installer is registered.
		true = lists:member( pricing, registered() ),
		
		% Schedule a conversion (mass replacement) with given name, on given services, to occur in given time-period.
		Group = "South Bogusdale",
		Service = "Test-Service-ID",
		Services = [ Service ],
		T1 = { date(), time() },
		T2 = { date(), time() },
		Period = { T1, T2 },
		installer ! { schedule_group, Group, with_services, [ Services ], for_period, Period }.
