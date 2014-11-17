%%%----------------------------------------------------------------------
%%% File    : mod_auth_log.erl
%%%----------------------------------------------------------------------

-module(mod_auth_log).
-author('mail@example.org').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, auth_success_log/3, auth_failure_log/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, _Opts) ->
	ejabberd_hooks:add(c2s_auth_success, Host, ?MODULE, auth_success_log, 100),
	ejabberd_hooks:add(c2s_auth_failure, Host, ?MODULE, auth_failure_log, 100),
	ok.

stop(Host) ->
	ejabberd_hooks:delete(c2s_auth_success, Host, ?MODULE, auth_success_log, 100),
	ejabberd_hooks:delete(c2s_auth_failure, Host, ?MODULE, auth_failure_log, 100),
	ok.

addlog(Timestamp, IP, JID, Source, Action) ->
	SUser = ejabberd_odbc:escape(JID#jid.user),
	LServer = JID#jid.server,
	SServer = ejabberd_odbc:escape(LServer),
	SSource = ejabberd_odbc:escape("Authentication source: " ++ Source),
	SAction = ejabberd_odbc:escape(Action),
	SIP = ejabberd_odbc:escape(jlib:ip_to_list(IP)),
	{{TYear, TMonth, TDay}, {THour, TMin, TSec}} = calendar:now_to_local_time(Timestamp),
	STimestamp = lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
											 [TYear, TMonth, TDay, THour, TMin, TSec])),
	odbc_queries:add_security_log(LServer,
		SUser, SServer, SIP, SAction, SSource, STimestamp),
	ok.

auth_success_log(IP, JID, Source) ->
	addlog(now(), IP, JID, Source, "auth_success").

auth_failure_log(IP, JID, Source) ->
	addlog(now(), IP, JID, Source, "auth_failure").
