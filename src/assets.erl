%%% Asset Manager (assets) :: Track services and metering network devices.
%%% 	assets ! { service, S, usage_monitored_by_device, D, starting_at_time, T, TimeStamp }
%%% 	assets ! { service, S, installed_at_location, L, TimeStamp }
%%%	  assets ! { service, S, removed_from_location, L, TimeStamp }
%%%	  assets ! { asset, A, has_attributes, Dict , TimeStamp } 
%%%	  assets ! { asset_class, C, has_attributes, Dict , TimeStamp }
%%%	  assets:get( {asset, A, attributes   ) -> orddict().
%%% where
%%%	  time() -> { date(), time() }
%%%	  location() -> { latitude, longitude } -- GPS coordinates
%%%	  service() and device() are asset identifiers (inventory serial numbers)
%%%	  Dict is maintained in a list via orddict.
%%% --------------------------------------------------------------
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green

-module( assets ).
-behaviour(gen_server).
-define( SERVER, ?MODULE ).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( NODEBUG, true ).   % Turn-off debug macros -- debugVal | debugMsg
-define( NOTEST, true ).    % Turn-off test macros. -- assert
-include_lib("eunit/include/eunit.hrl").

%-import( send, [send/2] ).

%% --------------------------------------------------------------------
%% External exports
-export( [start_server/0, ask/1 ] ).

%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-record( state, {} ).

%% ====================================================================
%% External functions (API)
%% ====================================================================

% REMOVED get( {asset, Asset, attributes } ) -> gen_server:call( ?SERVER, {asset, Asset, attributes } ).

ask( Question ) ->
		gen_server:call( ?SERVER, Question ).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_server() -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_server() ->
		?debugMsg( start_server),
		
		gen_server:start_link(                       % Start server instance.
                           { global, ?SERVER },  % Give it a global name.
													 ?MODULE, []           % The module containing and arguments for init/1.
													 , []                  % options
                           ).  

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
		?debugMsg( init ),	
		% Assure that quad has been globally registered -- Assume it is running.
		case global:whereis_name( quad ) of
				undefined ->
						 error_logger:warning_report({assets, init, quad_not_globally_registered });	
				PID when is_pid(PID) ->
						ok;
				_ ->
						 error_logger:warning_report({assets, init, quad_no_global_PID })
				end,
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call( Question, From, State ) ->
		?debugVal( { handle_call, Question, From } ),	
    Reply = answer( Question ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Message, State ) ->           % Asynchronous -- No value is returned.
		?debugVal( { handle_cast, Message } ),	
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( Action, State) ->
    ?debugVal( { handle_info, Action } ),	
		ok = do( Action ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate( _Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change( _OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% Actions

do( { service, S, usage_monitored_by_device, D, TimeStamp } ) -> 
		send( quad,  { begin_entity_relation, S, service_usage_monitored_by_device, D, TimeStamp } ),
		ok;

do( { service, S, installed_at_premise, P, TimeStamp } ) -> 
		send( quad,  { add_relationship, S, installed_at_premise, P, TimeStamp } ),
		ok;

do( { service, S, removed_from_premise, P, TimeStamp } ) -> 
		send( quad,  { end_relationship, S, installed_at_premise, P, TimeStamp } ),
		ok;

do( { asset, _Asset, has_attributes, [] , _TimeStamp } ) -> ok;

do( { asset, Asset, has_attributes, NameValueList , TimeStamp } ) -> 
		[ { AttributeName, Value } | Remainder ] = NameValueList,
		do( { asset, Asset, has_attribute, AttributeName, value, Value, TimeStamp } ),
		do( { asset, Asset, has_attributes, Remainder , TimeStamp }  ); 

do( { asset, Asset, has_attribute, AttributeName, value, Value, TimeStamp } ) ->
		send( quad,  { set_attribute, Asset, AttributeName, Value, TimeStamp } ),
		ok;

do( { asset_class, _Class, has_attributes, _NameValueList , _TimeStamp } ) -> 
		not_implemented_yet;

do( UnmatchedAction ) ->
		error_logger:error_report( { ?MODULE, does_not_understand_Request, UnmatchedAction } ).

%% Answer questions about assets.

answer( { what_is_value_of_attribute, AttributeName, for_asset, AssetID } ) ->
		Reply = world_base:ask( { what_is_current_value_of_attribute, AssetID, AttributeName } ),
		Reply;

answer( UnmatchedRequest ) ->
		{ ?MODULE, does_not_understand_Request, UnmatchedRequest }.
		




%% --------------------------------------------------------------------

send( { global, Name }, Cmd ) ->
		true = is_atom( Name ),
		?debugVal( { send, global, Name, Cmd } ),	
	  undefined =/= global:whereis_name( Name ),
		catch global:send( Name, Cmd ),
    ok;

send( M, Cmd) when is_pid( M ) ->
		?debugVal( {send, local, M, Cmd }),
    M ! Cmd,
    ok;

send( Name, Cmd) when is_atom( Name )->
		?debugVal( {send, local, Name, Cmd }),
		undefined =/= global:whereis_name( Name ),		
		catch global:send(Name, Cmd),
    ok.

