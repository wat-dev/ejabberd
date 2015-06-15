%%%----------------------------------------------------------------------
%%% File    : mod_announce.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage announce messages
%%% Created : 11 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% Implements a small subset of XEP-0133: Service Administration
%%% Version 1.1 (2005-08-19)

-module(mod_announce).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 init/0,
	 stop/1,
	 announce/3,
	 send_motd/1,
	 send_presence/1,
	 announce_motd/2,
	 send_announcement/2,
	 send_announcement_to_all/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(motd, {server, packet}).

-define(PROCNAME, ejabberd_announce).

start(Host, _Opts) ->
    mnesia:create_table(motd,
                        [{disc_copies, [node()]},
                         {attributes,
                          record_info(fields, motd)}]),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, announce, 50),
    ejabberd_hooks:add(user_available_hook, Host,
		       ?MODULE, send_motd, 50),
    ejabberd_hooks:add(user_available_hook, Host,
		       ?MODULE, send_presence, 50),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     proc_lib:spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
    	{announce_online, From, To, Body} ->
	    	announce_online(From, To, Body),
	    	loop();
		{announce_all_hosts_online, From, To, Body} ->
	    	announce_all_hosts_online(From, To, Body),
	    	loop();
	    {announce_motd, From, To, Body} ->
			announce_motd(From, To, Body),
			loop();
		_ ->
	    	loop()
    end.

stop(Host) ->
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
			  ?MODULE, announce, 50),
    ejabberd_hooks:delete(user_available_hook, Host,
			  ?MODULE, send_motd, 50),
    ejabberd_hooks:delete(user_available_hook, Host,
			  ?MODULE, send_presence, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

%% Announcing via messages to a custom resource
announce(From, To, Packet) ->
	{xmlelement, Name, _Attrs, _Els} = Packet,
    case Name of
		"message" ->
	    	Proc = gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME),
    		Body = xml:get_subtag_cdata(Packet, "body"),

	    	case Body of
	    		"online " ++ MeaningfulBody ->
	    			Proc ! {announce_online, From, To, MeaningfulBody},
	    			stop;
	    		"aho " ++ MeaningfulBody ->
	    			Proc ! {announce_all_hosts_online, From, To, MeaningfulBody},
	    			stop;
	    		"motd " ++ MeaningfulBody ->
	    			Proc ! {announce_motd, From, To, MeaningfulBody},
	    			stop;
	    		_ ->
	    			ok
	    	end;
		_ ->
	    	ok
    end.

%%-------------------------------------------------------------------------

report(From, To, Body, Type) ->
	case ejabberd_reporting:report([
				{from, jlib:jid_to_string(From)},
				{to, jlib:jid_to_string(To)},
				{body, Body},
				{type, Type}]) of
		{ok, Data} ->
			?INFO_MSG("Announcement ~s confirmed: ~p", [To#jid.resource, Data]),
			ok;
		{error, Data} ->
			?WARNING_MSG("Announcement ~s forbidden: ~p", [To#jid.resource, Data]),
			if
				is_binary(Data) ->
					binary_to_list(Data);
				is_list(Data) ->
					Data;
				true ->
					"the response may not be presented in a suitable form"
			end
	end.

allow_announcement(Host, From) ->
	case acl:match_rule(Host, ejabberd_announce, From) of
		deny ->
			false;
		allow ->
			true
	end.

run_announcement(From, To, Host, Body, Code) ->
    case allow_announcement(Host, From) of
    	false ->
    		ejabberd_router:route(To, From, get_packet("Forbidden by Ejabberd"));
    	true ->
    		case report(From, To, Body, lists:flatten(io_lib:format("~p", [Code]))) of
    			ok ->
    				Sessions = Code(Host, Body),
    				TargetUsersCount = length(Sessions),
    				Reply = lists:flatten(io_lib:format("Announcement sent to ~p users", [TargetUsersCount])),
    				ejabberd_router:route(To, From, get_packet(Reply));
    			Error ->
    				ejabberd_router:route(To, From, get_packet("Forbidden by CTS: " ++ Error))
    		end
    end.

announce_online(From, To, Body) ->
	run_announcement(From, To, To#jid.lserver, Body, fun send_announcement/2).

announce_all_hosts_online(From, To, Body) ->
	run_announcement(From, To, global, Body, fun send_announcement/2).

announce_motd(From, To, Body) ->
	run_announcement(From, To, To#jid.lserver, Body, fun announce_motd/2).

announce_motd(Server, Body) ->
	LServer = jlib:nameprep(Server),
    announce_motd_delete(LServer),

    if Body /= "delete" ->
    		FSet = fun() ->
                	mnesia:write(#motd{server = LServer, packet = Body})
        	end,
    		mnesia:transaction(FSet),

			send_announcement(LServer, Body);
    	true ->
    		[]
	end.

announce_motd_delete(LServer) ->
    F = fun() ->
            mnesia:delete({motd, LServer})
    end,
    mnesia:transaction(F).

send_motd(#jid{lserver = LServer} = JID) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Body}] ->
		Packet = get_packet(Body),
		Local = get_from(LServer),
		ejabberd_router:route(Local, JID, Packet);
	_ ->
	    ok
    end.

send_presence(JID) ->
	Presence = {xmlelement, "presence", [], [
			{xmlelement, "status", [], [
					{xmlcdata, "[ online | aho | motd ] <message>"}
				]}
		]},
	lists:foreach(fun(Host) ->
				case allow_announcement(Host, JID) of
					true ->
						FromJID = jlib:make_jid("", Host, "Announce"),
						ejabberd_router:route(FromJID, JID, Presence);
					false ->
						ok
				end
		end, ?MYHOSTS),
	ok.

%% ejabberd_admin compatibility
send_announcement_to_all(_Host, _SubjectS, Body) ->
	Host = global,
	send_announcement(Host, Body),
	ok.

get_from(global) ->
	jlib:make_jid("", ?MYNAME, "");
get_from(Host) ->
	jlib:make_jid("", Host, "").

get_packet(Body) ->
	if is_tuple(Body) ->
			Body;
		true ->
			{xmlelement,
				"message", [{"type", "chat"}],
				[
					{xmlelement,
						"body", [],
						[
							{xmlcdata, Body}
						]
					}
				]}
	end.

send_announcement(ToHost, Body) ->
	send_announcement(get_from(ToHost), ToHost, Body).

send_announcement(FromJID, ToHost, Body) ->
	Packet = get_packet(Body),
	Sessions = if ToHost /= global ->
			ejabberd_sm:get_vh_session_list(ToHost);
		true ->
			ejabberd_sm:dirty_get_sessions_list()
	end,
	lists:foreach(
		fun({U, S, R}) ->
				ToJID = jlib:make_jid(U, S, R),
				ejabberd_router:route(FromJID, ToJID, Packet)
		end, Sessions),
	Sessions.
