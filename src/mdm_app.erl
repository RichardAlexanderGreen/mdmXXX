%%% @doc This is the _app module of the MDM application.  CURRENTLY JUST A TEMPLATE
%%%      The mdm_server module contains a detailed description of the application.
%%% @end
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green
-module(mdm_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports - Application behavior uses these call-back functions
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% This function is called when OTP wants to start the application.
%% As a minimum, we start the root supervisor.
%% Other startup tasks may be appropriate here as well.
%% Type :: normal | {failover, } | { takerover, }
%% StartArgs :: arguments provided in __.app file.
%% --------------------------------------------------------------------
start( _Type, StartArgs ) ->
	?debugMsg( start ),
  case mdm_sup:start_supervisor( StartArgs ) of    % Start root supervisor.
	  {ok, Pid} ->
	    {ok, Pid};   % Return the root supervisor's PID.
  Error ->
	    { start_supervisor_not_okay, Error }
  end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop( _State ) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

