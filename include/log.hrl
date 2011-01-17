%% -----------------------------------------------------------------------------------
%% Log events for debug.

% Reports below the current LOGLEVEL are ignoared.
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
log( trace, _Report ) when ?LOGLEVEL > 0 -> ok;   % Ignore info level.
log( debug, _Report ) when ?LOGLEVEL > 1 -> ok;   % Ignore info level.
log(  info, _Report ) when ?LOGLEVEL > 2 -> ok;   % Ignore debug level.
log(  warn, _Report ) when ?LOGLEVEL > 3 -> ok;   % Ignore debug level.
log(warning,_Report ) when ?LOGLEVEL > 3 -> ok;   % Ignore debug level.
log( error, _Report ) when ?LOGLEVEL > 4 -> ok;   % Ignore debug level.
log( fatal, _Report ) when ?LOGLEVEL > 5 -> ok;   % Ignore debug level.
log(_Level, _Report ) when ?LOGLEVEL > 6 -> ok;   % Ignore debug level.

log( Level, Report ) ->
		% If DEBUG is on also write to console.
		case ?DEBUG of
				true -> io:format("~w.~n", [{ udt(), Level, Report }] ) end,
						
%		ets:insert( get(log), { udt(), Level, Report } ).  %% Consider using file:write/2.
    ok = file:write( get(log), io_lib:format("~w,~n", [{ udt(), Level, Report }] )  ),
		ok.

log_open( LogName ) ->
		%initialize_table( LogName, duplicate_bag ),
		{ ok, IO_Device } = file:open( LogName, [append] ),
		put( log, IO_Device ),
		ok.

log_close() ->
		%flush_each_table(  [ get( log ) ]  ),
		file:close( get( log )  ),
		ok.


% Thinking out loud: I prefer file:write/2 because it can be read outside of erlang.
% If necessary, the log file can be formatted to enable some external log scanner.
% ets enables qlc queries, which can be useful when scanning a large log.
% ets can also require a large memory space if the log is large.
% With both systems, you have to deal with the fact that the io is buffered.

%% -----------------------------------------------------------------------------------
