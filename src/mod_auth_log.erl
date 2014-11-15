%%%----------------------------------------------------------------------
%%% File    : mod_auth_log.erl
%%%----------------------------------------------------------------------

-module(mod_auth_log).
-author('mail@example.org').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, init/1, auth_success_log/3, auth_failure_log/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_auth_log).

start(Host, Opts) ->
	LogFileName = gen_mod:get_opt(log_file_name, Opts, "/tmp/ejabberd_auth.log"),

	ejabberd_hooks:add(c2s_auth_success, Host, ?MODULE, auth_success_log, 100),
	ejabberd_hooks:add(c2s_auth_failure, Host, ?MODULE, auth_failure_log, 100),

	register(gen_mod:get_module_proc(Host, ?PROCNAME),
			 spawn(?MODULE, init, [LogFileName])).

stop(Host) ->
	ejabberd_hooks:delete(c2s_auth_success, Host, ?MODULE, auth_success_log, 100),
	ejabberd_hooks:delete(c2s_auth_failure, Host, ?MODULE, auth_failure_log, 100),
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	Proc ! stop,
	{wait, Proc}.

init(LogFileName) ->
	{ok, Io} = file:open(LogFileName, [append]),
	loop(Io).

loop(Io) ->
	receive
		{addlog, {Timestamp, IP, JID, Source, Status}} ->
			SJID = jlib:jid_to_string(JID),
			SIP = jlib:ip_to_list(IP),
			{{TYear, TMonth, TDay}, {THour, TMin, TSec}} = calendar:now_to_local_time(Timestamp),
			STimestamp = lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
													 [TYear, TMonth, TDay, THour, TMin, TSec])),
			io:fwrite(Io, "[~s] ~s ~s as ~s (via ~s)~n",
					  [STimestamp, SIP, Status, SJID, Source]),
			loop(Io);
		stop ->
			file:close(Io),
			ok;
		_ ->
			loop(Io)
	end.

auth_success_log(IP, JID, Source) ->
	Host = JID#jid.server,
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	Proc ! {addlog, {now(), IP, JID, Source, "succeeded"}}.

auth_failure_log(IP, JID, Source) ->
	Host = JID#jid.server,
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	Proc ! {addlog, {now(), IP, JID, Source, "failed"}}.
