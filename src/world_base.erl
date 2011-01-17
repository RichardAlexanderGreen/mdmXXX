%%% world_base implementation
%%% Notices received:
%%%  world_base ! { set_entity_attribute_value, Entity, AttributeName, Value, TimeStamp }
%%%  world_base ! { begin_entity_relation, EntityA, RelationName, EntityB, TimeStamp }
%%%  world_base ! { end_entity_relation, EntityA, RelationName, EntityB, TimeStamp }
%%% Business Event correlated to TimeStamp above - who did what, where, when, and why.
%%%  world_base ! { w5, TimeStampWhen, Who, What, Where, Why }
%%% Queries answered:
%%%  world_base:get( Entity, Attribute ) -> CurrentValue
%%%  world_base:getHistory( Entity, Attribute ) -> ListOfTransactions
%%% Created : Dec 13, 2010
%%% -------------------------------------------------------------------
-module(world_base).

-behaviour(gen_server).
-define( SERVER, ?MODULE ).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( DEBUG, true ).
-define( TEST, true ).
-include_lib("eunit/include/eunit.hrl").


%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).
% My API
-export( [ ask/1 ]  ).
-export( [ create_world_base/1 ]  ).
-export( [ start_server/0 ]  ).


-record( state, { n_set_entity_attribute = 0
								, n_get_entity_attribute = 0 
								, n_begin_entity_relation = 0
								, n_end_entity_relation = 0
								} ).
-record( entity_attribute, { entity_attribute, value_history } ). 
% entity_attribute is {entity, attribute}.
% value_history is [ { value, timeStamp } ] .  

-record( entity_relationship, { entity_relation, related_entities } ).
% entity_relation is {entity, relation}.
% related_entities is [ { entity, timeStamp} ]

-record( w5, { timestampWhen, who, what, where, why } ). 
% w5 :: standard log entry for world-base transactions.
% when: timestamp :: generated from { now(), self() }
% who: actor = user | application.
% what: event-type.
% where: function creating the w5 (module:function/arity)
% why: job-ticket


%% ====================================================================
%% External functions
%% ====================================================================
ask( Question ) ->
		gen_server:call( ?SERVER, Question ).
		
% Call only when you want to create a new Mnesia database on the current node.
create_world_base( Password ) ->
		Password = '734-223-6687',
		_ = mnesia:create_schema([node()]),
		mnesia:start(),
		TableConfigs = [ { disc_only_copies, [node()]  }
									 , { type, set } 
									 ],
		{atomic, ok} = mnesia:create_table( entity_attribute, [ {attributes, record_info( fields, entity_attribute ) } ]  
																			++ TableConfigs 
																			),
		{atomic, ok} = mnesia:create_table( entity_relationship, [ {attributes, record_info( fields, entity_relationship ) } ]  
																			++ TableConfigs 
																			),
		{atomic, ok} = mnesia:create_table( w5, [ {attributes, record_info( fields, w5 ) } ]  
																			++ TableConfigs 
																			),
		%mnesia:stop(),
		mnesia:table_info( entity_attribute, all ).
%,ok.


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
                           { global, ?SERVER },  % Give it a local name.
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
		InitialState = #state{},  % Zero Counts	
    {ok, InitialState }.

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
handle_call( state, _From, State ) ->
		{reply, State, State };

handle_call( Question, From, State ) ->
		?debugVal( { handle_call, Question, From }),	
    Reply = answer( Question ),
		NewState = #state{ n_set_entity_attribute  = State#state.n_set_entity_attribute
					           , n_get_entity_attribute  = State#state.n_get_entity_attribute + 1
					           , n_begin_entity_relation = State#state.n_begin_entity_relation
					           , n_end_entity_relation   = State#state.n_end_entity_relation 
					           },
    {reply, Reply, NewState}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Action, State) ->
		?debugVal( { handle_cast, Action, State }),		
		UpdatedState = do( Action, State ),
    { noreply, UpdatedState }.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( Action, State ) ->
		?debugVal( {handle_info, Action, State } ),
		UpdatedState = do( Action, State ),
    { noreply, UpdatedState }.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% Do actions.

do( { set_entity_attribute_value, Entity, AttributeName, Value, TimeStamp } , State ) ->
		PutAttribute = fun() -> 
				Key = { Entity, AttributeName },
				OldRecord = mnesia:read( entity_attribute, Key, write ),
				?debugVal( OldRecord ),
				case OldRecord of
						[] ->
							  ValueHistory = [ { Value, TimeStamp} ],
								Record = #entity_attribute{ entity_attribute = Key, value_history = ValueHistory },
								mnesia:write( Record );
						
						[ {entity_attribute, Key, OldHistory} ] ->
								%?debugVal( OldHistory ),
								ValueHistory = [ { Value, TimeStamp }  ]  ++ OldHistory,
							  Record = #entity_attribute{ entity_attribute = Key, value_history = ValueHistory },
								mnesia:write( Record );
						
						HowDidWeGetHere ->
								?debugVal( HowDidWeGetHere ),
								mnesia:abort( {put_attribute_case_fell_thru, HowDidWeGetHere} )
				end
		end,
		{atomic, ok} = mnesia:transaction( PutAttribute ),
		#state{ n_set_entity_attribute  = State#state.n_set_entity_attribute + 1
					, n_get_entity_attribute  = State#state.n_get_entity_attribute 
					, n_begin_entity_relation = State#state.n_begin_entity_relation
					, n_end_entity_relation   = State#state.n_end_entity_relation 
					};

do( { dirty_entity_attribute_value, Entity, AttributeName, Value, TimeStamp } , State ) ->
		%PutAttribute = fun() -> 
				Key = { Entity, AttributeName },
				OldRecord = mnesia:dirty_read( entity_attribute, Key ),
				?debugVal( OldRecord ),
				case OldRecord of
						[] ->
							  ValueHistory = [ { Value, TimeStamp} ],
								Record = #entity_attribute{ entity_attribute = Key, value_history = ValueHistory },
								mnesia:dirty_write( Record );
						
						[ {entity_attribute, Key, OldHistory} ] ->
								%?debugVal( OldHistory ),
								ValueHistory = [ { Value, TimeStamp }  ]  ++ OldHistory,
							  Record = #entity_attribute{ entity_attribute = Key, value_history = ValueHistory },
								mnesia:dirty_write( Record );
						
						HowDidWeGetHere ->
								?debugVal( HowDidWeGetHere ),
								mnesia:abort( {put_attribute_case_fell_thru, HowDidWeGetHere} )
				end
		%end,
		%{atomic, ok} = mnesia:transaction( PutAttribute ),
		#state{ n_set_entity_attribute  = State#state.n_set_entity_attribute + 1
					, n_get_entity_attribute  = State#state.n_get_entity_attribute 
					, n_begin_entity_relation = State#state.n_begin_entity_relation
					, n_end_entity_relation   = State#state.n_end_entity_relation 
					};

do( { begin_entity_relation, EntityA, RelationName, EntityB, TimeStamp }  , State ) ->
		PutRelation = fun() ->
												Key = { EntityA, RelationName },
												OldRecord = mnesia:read( entity_relationship, Key, write ),
												%?debugVal( OldRecord ),
												case OldRecord of
														[] ->
																RelationList = [ { EntityB, TimeStamp} ],
																Record = #entity_relationship{ entity_relation = Key, related_entities = RelationList },
																mnesia:write( Record );
														[ {entity_relationship, Key, CurrentList} ] ->
																?debugVal( CurrentList ),
																RelationList = [ { EntityB, TimeStamp }  ]  ++ CurrentList,
																Record = #entity_relationship{ entity_relation = Key, related_entities = RelationList },
																mnesia:write( Record );
														HowDidWeGetHere ->
																?debugVal( HowDidWeGetHere ),
																mnesia:abort( {put_relation_case_fell_thru, HowDidWeGetHere} )
												end
									end,
		{atomic, ok} = mnesia:transaction( PutRelation ),
		#state{ n_set_entity_attribute  = State#state.n_set_entity_attribute
					, n_get_entity_attribute  = State#state.n_get_entity_attribute 
					, n_begin_entity_relation = State#state.n_begin_entity_relation + 1
					, n_end_entity_relation   = State#state.n_end_entity_relation 
					};


% do( { end_entity_relation, EntityA, RelationName, EntityB, TimeStamp }  , State ) ->
%	 	not_implemented;

do( { entity, EntityID, attribute, AttributeName, has_value, Value, TimeStamp } , State  ) ->
		 % This is same as:
		 do( { set_entity_attribute_value, EntityID, AttributeName, Value, TimeStamp } , State  );

do( Action , State  ) ->
		exit( { {?MODULE, does_not_understand_Action, Action}, {state_at_exit, State} } ).

%% Answer questions.
answer( { what_is_current_value_of_attribute, EntityID, Attribute }) ->
		%?debugVal( { what_is_current_value_of_attribute, EntityID, Attribute } ),
		
		GetValue = fun() ->
										mnesia:read( entity_attribute, { EntityID, Attribute }  )
							 end,
		Result = mnesia:transaction( GetValue ),
		%?debugVal( Result ),
		case Result of
				{ atomic, [] } ->
						{ ok, none };
				
				{ atomic, RecordReturned } ->
						[ { entity_attribute, {EntityID, Attribute}, Value_History } ] = RecordReturned,
						[ { Value, _TimeStamp } | _ ] = Value_History,
						{ok, Value };
				{ aborted, Reason } ->
						exit( { what_is_current_value_of_attribute_failed, Reason } )
		end;

answer( {which_entities_are_current_in_relation, Entity, Relation } ) ->
		%?debugVal( {which_entities_are_current_in_relation, Entity, Relation } ),
		
		GetValue = fun() ->
										mnesia:read( entity_Relation, { Entity, Relation }  )
							 end,
		Result = mnesia:transaction( GetValue ),
		%?debugVal( Result ),
		case Result of
				{ atomic, [] } ->
						{ ok, none };
				
				{ atomic, RecordReturned } ->
						[ { entity_Relation, { Entity, Relation}, Relation_List } ] = RecordReturned,
						[ { Value, _TimeStamp } | _ ] = Relation_List,
						{ok, Value };
				{ aborted, Reason } ->
						exit( { which_entities_are_current_in_relation, Reason } )
		end;

answer( Request ) ->
		{ ?MODULE, does_not_understand_Request, Request }.


%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

-ifdef( TEST ).

set_entity_attribute_value_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name',
		Value = "Test Attribute Value",
		TimeStamp = now(),
		%Service = whereis( world_base ),
		world_base ! { set_entity_attribute_value, Entity, AttributeName, Value, TimeStamp }.

set_entity_attribute_value2_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name 2',
		Value = "Test Attribute Value 2",
		TimeStamp = now(),
		%Service = whereis( world_base ),
		world_base ! { set_entity_attribute_value, Entity, AttributeName, Value, TimeStamp }.

get_entity_attribute_value_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name',
		ValueExpected = "Test Attribute Value",
		{ok, ValueSeen } = world_base:ask( { what_is_current_value_of_attribute, Entity, AttributeName }),
		?assertEqual( ValueExpected, ValueSeen ).

get_entity_attribute_value2_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name 2',
		ValueExpected = "Test Attribute Value 2",
		{ok, ValueSeen } = world_base:ask( { what_is_current_value_of_attribute, Entity, AttributeName }),
		?assertEqual( ValueExpected, ValueSeen ).

begin_entity_relation_test() ->
		EntityA = "Test EntityA ID",
		RelationName = 'test relationship name',
		EntityB = "Test EntityB ID",
		TimeStamp = now(),
		world_base ! { begin_entity_relation, EntityA, RelationName, EntityB, TimeStamp }.

begin_entity_relation2_test() ->
		EntityA = "Test EntityA ID",
		RelationName = 'test relationship name 2',
		EntityB = "Test EntityB ID 2",
		TimeStamp = now(),
		world_base ! { begin_entity_relation, EntityA, RelationName, EntityB, TimeStamp }.

dirty_entity_attribute_value_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name',
		Value = "Test Attribute Value",
		TimeStamp = now(),
		%Service = whereis( world_base ),
		world_base ! { dirty_entity_attribute_value, Entity, AttributeName, Value, TimeStamp }.

dirty_entity_attribute_value2_test() ->
		Entity = "Test Entity ID",
		AttributeName = 'test attribute name 2',
		Value = "Test Attribute Value 2",
		TimeStamp = now(),
		%Service = whereis( world_base ),
		world_base ! { dirty_entity_attribute_value, Entity, AttributeName, Value, TimeStamp }.

-endif.

