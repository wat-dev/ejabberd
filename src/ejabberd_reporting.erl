-module(ejabberd_reporting).
-author('unknown@example.org').

%% External exports
-export([
		report/1,
		report/2
	]).

-include("ejabberd.hrl").

inets_start() ->
	case inets:start() of
		ok ->
			ok;
		{error, {already_started, inets}} ->
			ok;
		Error ->
			Error
	end.

report(Data) ->
	report(Data, "reporting not configured").

report(Data, Default) ->
    case inets_start() of
    	ok ->
    		ReportingSettings = ejabberd_config:get_local_option(reporting),
    		case ReportingSettings of
    			undefined ->
    				{ok, Default};
    			_ ->
    				URL = proplists:get_value(url, ReportingSettings),
    				AuthToken = erlang:atom_to_list(erlang:get_cookie()),
    				Timeout = proplists:get_value(processing_timeout, ReportingSettings, 2000),
    				ConnectTimeout = proplists:get_value(connect_timeout, ReportingSettings, 500),
    				Result = httpc:request(post,
    					{
    						URL,
    						[
    							{"Auth-Token", AuthToken},
    							{"Accept", "application/bert"}
    						],
    						"application/bert",
    						erlang:term_to_binary({bert, dict, Data})
    					},
    					[{timeout, Timeout}, {connect_timeout, ConnectTimeout}],
    					[{body_format, binary}]),
    				case Result of
    					{ok, {{_HTTPVersion, 200, "OK"}, _Headers, Body}} ->
    						case catch erlang:binary_to_term(Body) of
    							{'EXIT', DataReadError} ->
    								?WARNING_MSG("Cannot parse reporting result from ~s: ~p",
    									[URL, DataReadError]),
    								{ok, Default};
    							ParsedTerm ->
    								{ok, ParsedTerm}
    						end;
    					{ok, {{_HTTPVersion, ErrorCode, ErrorLine}, _Headers, Body}} ->
    						?WARNING_MSG("Reporting server ~s reports an error: ~w ~s", [URL, ErrorCode, ErrorLine]),
    						case catch erlang:binary_to_term(Body) of
    							{'EXIT', DataReadError} ->
    								?WARNING_MSG("Cannot parse failed reporting result from ~s: ~p",
    									[URL, DataReadError]),
    								{error, Default};
    							ParsedTerm ->
    								{error, ParsedTerm}
    						end;
    					{Status, StatusInfo} ->
    						?WARNING_MSG("Reporting server ~s is not available (error ~p): ~p", [URL, Status, StatusInfo]),
    						{error, Default}
    				end
    		end;
    	Error ->
    		Error
    end.
