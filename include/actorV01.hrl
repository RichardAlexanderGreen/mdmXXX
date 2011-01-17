%% Copyright: Richard Alexander Green 2011
%% Created: Jan 6, 2011
%% Description: actor.hrl provides the Erlang general server wrapper
%%  that every business process actor will need.
%%  The defines adds do/2 and answer/1 functions 
%%  to handle asynchronous requests and synchronous queries.

-behaviour( gen_server ).
-define( SERVER, ?MODULE ).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( NOTEST, true ).
-define( NODEBUG, true ).
-define( LOGLEVEL, 3 ).  % LOG info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


%% --------------------------------------------------------------------
%% External exports
-export([start_server/0]).

%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-record( state, { }   ).

%% ====================================================================
%% External functions
%% ====================================================================
%%% Queries:


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
		?debugVal( {start_server, enter} ),
		
		Result = gen_server:start_link(              % Start server instance.
                           { local, ?SERVER }    % Give it a LOCAL name -- enable use of !
													 ,?MODULE              % The module containing init/1
												   , [ { host, node() } ]  % parameters for init/1.
													 , []                  % options
                           ),
		?debugVal( { start_link_result, Result } ),
		Result.
		
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(ArgList) ->
		
		process_flag( trap_exit, true ),   % Enable terminate -- So we can flush tables on shutdown
		
%		Host = lists:keyfind( host, 1, ArgList ),
%		log( info, { put_host, Host } ),	
%		put( host, Host ),

		global:register_name( quad, self() ),    % Enable remote nodes to use global:send( quad, {Action} )
		
		% Call actor's initialization routine.
		State = custom_init( ArgList ),
		
    {ok, State }.

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
		
handle_call( Request, From, State) ->     % Synchronous -- Caller is waiting for reply.
		log( info, { handle_call, Request, From} ),	
    Reply = answer( Request, From, State ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Action, State ) ->           % Asynchronous -- No value is returned.
		log( info, { handle_cast, Action } ),	
		ok = do( Action, State ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( Action, State ) ->
		?debugVal( { handle_info, Action }  ),
    %log( info, { handle_info, Action }  ),	
		State2 = do( Action, State ),   % Pass State because it has Table ID in it.
		{noreply, State2}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate( shutdown, State ) ->
		custom_shutdown( State ),
		ok;

terminate( _Reason, _State) ->
    log( info, { terminate_reason, _Reason } ),
		ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change( _OldVsn, State, _Extra) ->
    {ok, State}.

log( Type, Any ) ->
		ok.

