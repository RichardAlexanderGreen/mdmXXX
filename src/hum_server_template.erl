%%% @doc This is a server template.
%%%      The mdm_protocol module demonstrates how each function is called.
%%% Notices:
%%%
%%% Queries:
%%%
%%% Requests:
%%%
%%% @end
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green

-module( hum_server_template ).
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
		
		gen_server:start_link(  % Start server instance.
                           { local, ?SERVER },    % Give it a local name.
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
handle_call( Request, From, State) ->     % Synchronous -- Caller is waiting for reply.
		?debugVal( { handle_call, Request, From} ),	
    Reply = answer( Request ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Action, State ) ->           % Asynchronous -- No value is returned.
		?debugVal( { handle_cast, Action } ),	
		ok = do( Action ),
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

do( Action ) ->
		{ ?MODULE, does_not_understand_action, Action }.

answer( Request ) ->
		{ ?MODULE, does_not_understand_request, Request }.
