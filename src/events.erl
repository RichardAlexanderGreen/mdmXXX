%%% Event History (events) :: Record service/device event history.
%%%	  events ! { device, D, reports_event, E, on_service, S }
%%% where
%%% 	event() -> { event_type(), event_time(), details() } 
%%% --------------------------------------------------------------
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green

-module( events ).
-behaviour(gen_server).
-define( SERVER, ?MODULE ).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_server/0]).

%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-record( state, {} ).

%% ====================================================================
%% External functions
%% ====================================================================
%%% Notices:

%%% Tasks:

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
handle_call( Request, _From, State) ->     % Synchronous -- Caller is waiting for reply.
		?debugVal( { handle_call, Request} ),	
    Reply = ok,
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
handle_info( Info, State) ->
    ?debugVal( { handle_info, Info } ),	
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

