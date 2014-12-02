%%%----------------------------------------------------------------------
%%% File    : ejabberd_xmlrpc.erl
%%%----------------------------------------------------------------------

-module(ejabberd_xmlrpc).
-author('mail@example.org').

-export([
	 start_listener/2,
	 handler/2,
	 handler/3,
	 socket_type/0
	]).

-include("ejabberd.hrl").
-include("mod_muc/mod_muc_room.hrl").
-include("jlib.hrl").

%% Copied from mod_muc/mod_muc.erl
-record(muc_online_room, {name_host, pid}).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

-record(xmlrpc_state, {auth_code}).

start_listener({Port, _Ip, tcp = _TranportProtocol}, Opts) ->
    %% get options
    MaxSessions = gen_mod:get_opt(maxsessions, Opts, 10),
    Timeout = gen_mod:get_opt(timeout, Opts, 5000),
    AuthCode = gen_mod:get_opt(auth_code, Opts, "t9n28439843tm83u9pxu321"),

    %% start the XML-RPC server
    Handler = {?MODULE, handler},
    xmlrpc:start_link(all, Port, MaxSessions, Timeout, Handler, #xmlrpc_state{auth_code = AuthCode}).

socket_type() ->
    independent.

handler(_State, {call, echothis, [A]}) ->
    {false, {response, [A]}};

handler(_State, {call, multhis, [{struct, [{a, A}, {b, B}]}]}) ->
    {false, {response, [A*B]}};

handler(State, {call, Method, [{struct, [{auth_code, AuthCode} | Args]}]}) ->
	ValidAuthCode = State#xmlrpc_state.auth_code,
	case AuthCode of
		ValidAuthCode ->
			handler(State, auth, {call, Method, [{struct, Args}]});
		_ ->
			{false, {response, [-1]}}
	end;

handler(_State, Payload) ->
	?WARNING_MSG("unauthorized call: ~p~n", [Payload]),
	{false, {response, ["Unauthorized call"]}}.

handler(_State, auth, {call, authenticate, [{struct, [{host, Host}, {username, UserName}, {password, Password}]}]}) ->
	Response = case ejabberd_auth:check_password(UserName, Host, Password) of
		true -> 1;
		_ -> 0
	end,
	{false, {response, [Response]}};

handler(_State, auth, {call, muc_list, [{struct, [{host, QueryHost}]}]}) ->
    Rooms = ets:tab2list(muc_online_room),
    {false, {response, [{array, lists:foldl(
                		fun({_, {Roomname, Host}, Pid}, Results) ->
                        		case QueryHost of
                            		"" ->
                            			[room_brief_struct(Roomname, Host, room_state(Pid)) | Results];
                            		Host ->
                            			[room_brief_struct(Roomname, Host, room_state(Pid)) | Results];
                            		_ ->
                                		Results
                        		end
                		end,
                		[],
                		Rooms)}]}};

handler(_State, auth, {call, muc_create, [{struct, [
													{room, UnsafeRoom},
													{host, UnsafeHost},
													{vhost, UnsafeVHost},
													{creator, UnsafeCreator}
												   ]}]}) ->
	Room = normalize(UnsafeRoom),
	Host = normalize(UnsafeHost),
	VHost = normalize(UnsafeVHost),
	Creator = jlib:string_to_jid(normalize(UnsafeCreator)),
	case Creator of
		error ->
			{false, {response, [0]}};
		_ ->
			case mnesia:dirty_read(muc_online_room, {Room, Host}) of
				[] ->
					case mod_muc:create_room(VHost, Room, Creator, Room, default) of
						ok ->
							{false, {response, [1]}};
						_ ->
							{false, {response, [0]}}
					end;
				_ ->
					{false, {response, [0]}}
			end
	end;

handler(_State, auth, {call, muc_set_owner, [{struct, [
		{room, UnsafeRoom},
		{host, UnsafeHost},
		{owner, UnsafeOwner}
	]}]}) ->
	Room = normalize(UnsafeRoom),
	Host = normalize(UnsafeHost),
	Owner = jlib:jid_tolower(jlib:jid_remove_resource(jlib:string_to_jid(normalize(UnsafeOwner)))),
	case mnesia:dirty_read(muc_online_room, {Room, Host}) of
		[R] ->
			Pid = R#muc_online_room.pid,
			gen_fsm:send_all_state_event(Pid, {set_owner, Owner}),
			{false, {response, [1]}};
		_ ->
			{false, {response, [0]}}
	end;

handler(_State, auth, {call, muc_destroy, [{struct, [
					{room, UnsafeRoom},
					{host, UnsafeHost},
					{reason, UnsafeReason}
				]}]}) ->
	Room = normalize(UnsafeRoom),
	Host = normalize(UnsafeHost),
	Reason = normalize(UnsafeReason),
	case mnesia:dirty_read(muc_online_room, {Room, Host}) of
    	[R] ->
        	Pid = R#muc_online_room.pid,
        	case Reason of
        		"" ->
        			gen_fsm:send_all_state_event(Pid, destroy);
        		_ ->
        			gen_fsm:send_all_state_event(Pid, {destroy, Reason})
        	end,
        	{false, {response, [1]}};
    	[] ->
    		{false, {response, [0]}}
    end;

handler(_State, auth, {call, muc_info, [{struct, [
					{room, UnsafeRoom},
					{host, UnsafeHost}
				]}]}) ->
	Room = normalize(UnsafeRoom),
	Host = normalize(UnsafeHost),
	case mnesia:dirty_read(muc_online_room, {Room, Host}) of
    	[R] ->
        	Pid = R#muc_online_room.pid,
        	{false, {response, [room_detailed_struct(Room, Host, room_state(Pid), room_config(Pid))]}};
    	[] ->
    		{false, {response, [0]}}
    end;

handler(_State, auth, {call, announce_send, [{struct, [
					{vhost, UnsafeVHost},
					{subject, UnsafeSubject},
					{body, UnsafeBody}
				]}]}) ->
	VHost = normalize(UnsafeVHost),
	Subject = normalize(UnsafeSubject),
	Body = normalize(UnsafeBody),
	Packet = {xmlelement, "message", [{"type", "chat"}],
		if Subject /= [] ->
				[{xmlelement, "subject", [],
						[{xmlcdata, Subject}]}];
			true ->
				[]
		end ++
		if Body /= [] ->
				[{xmlelement, "body", [],
						[{xmlcdata, Body}]}];
			true ->
				[]
		end},
	Sessions = ejabberd_sm:get_vh_session_list(VHost),
	From = jlib:make_jid("", VHost, ""),
    lists:foreach(
      	fun({U, S, R}) ->
          		To = jlib:make_jid(U, S, R), 
          		ejabberd_router:route(From, To, Packet)
      	end, Sessions),
    {false, {response, [length(Sessions)]}};

handler(_State, auth, {call, c2s_list, [{struct, [
		            {server, UnsafeServer}
		        ]}]}) ->
	Server = normalize(UnsafeServer),
	MatchSpec = case Server of
		"@all" ->
			[{'_', [], ['$_']}];
		_ ->
			[{#session{usr = '$1', _ = '_'},
					[{'==', {element, 2, '$1'}, Server}],
					['$_']}]
	end,
	{false, {response, [{array,
		lists:map(fun(Session) ->
				{CurrentUser, CurrentServer, CurrentResource} = Session#session.usr,
				CurrentNode = atom_to_list(node(element(2, Session#session.sid))),
				{struct, [
						{user, CurrentUser},
						{server, CurrentServer},
						{resource, CurrentResource},
						{node, CurrentNode},
						{ip, jlib:ip_to_list(proplists:get_value(ip, Session#session.info))},
						{conn, atom_to_list(proplists:get_value(conn, Session#session.info))}
					]}
		end, mnesia:dirty_select(session, MatchSpec))}]}};

handler(_State, auth, {call, c2s_stop, [{struct, [
		{user, UnsafeUser},
		{server, UnsafeServer},
		{resource, UnsafeResource}
	]}]}) ->
	User = normalize(UnsafeUser),
	Server = normalize(UnsafeServer),
	Resource = normalize(UnsafeResource),
	USR = {User, Server, Resource},
	case mnesia:dirty_index_read(session, USR, #session.usr) of
		[] ->
			{false, {response, [0]}};
		Ss ->
			Session = lists:max(Ss),
			{_, Pid} = Session#session.sid,
			Pid ! replaced,
			{false, {response, [1]}}
	end;

handler(_State, auth, {call, s2s_list, [{struct, [
					{dir, UnsafeDir}
				]}]}) ->
	Infos = s2s_infos(UnsafeDir),
	{false, {response, [{array,
					lists:map(fun(StateInfos) ->
								CurrentDir = proplists:get_value(direction, StateInfos),
								case CurrentDir of
									out ->
										From = proplists:get_value(myname, StateInfos),
										To = proplists:get_value(server, StateInfos);
									in ->
										From = {array, proplists:get_value(domains, StateInfos)},
										To = proplists:get_value(myname, StateInfos)
								end,
								CurrentPortValue = proplists:get_value(port, StateInfos),
								CurrentPort = case is_atom(CurrentPortValue) of
									true ->
										atom_to_list(CurrentPortValue);
									_ ->
										CurrentPortValue
								end,
								CurrentStreamIDValue = proplists:get_value(streamid, StateInfos),
								CurrentStreamID = case is_atom(CurrentStreamIDValue) of
									true ->
										atom_to_list(CurrentStreamIDValue);
									_ ->
										CurrentStreamIDValue
								end,
								{struct, [
										{direction, atom_to_list(CurrentDir)},
										{state, atom_to_list(proplists:get_value(statename, StateInfos))},
										{addr, jlib:ip_to_list(proplists:get_value(addr, StateInfos))},
										{port, CurrentPort}, 
										{stream_id, CurrentStreamID},
										{from, From},
										{to, To},
										{tls, proplists:get_value(tls_enabled, StateInfos)}
									]}
						end, Infos)}]}};

handler(_State, auth, {call, s2s_stop, [{struct, [
				{dir, UnsafeDir},
				{stream_id, UnsafeStreamID}
			]}]}) ->
	Infos = s2s_infos(UnsafeDir),
	StreamID = normalize(UnsafeStreamID),
	StoppedCount = lists:foldl(fun(StateInfos, A) ->
				case proplists:get_value(streamid, StateInfos) of
					StreamID ->
						Pid = proplists:get_value(s2s_pid, StateInfos),
						gen_fsm:send_event(Pid, {xmlstreamend, whatever}),
						A + 1;
					_ ->
						A
				end
		end, 0, Infos),
	{false, {response, [StoppedCount]}};

handler(_State, auth, {call, s2s_filter, [{struct, [
					{vhost, UnsafeVHost},
					{server, UnsafeServer},
					{action, Action}
				]}]}) ->
	VHost = normalize(UnsafeVHost),
	Server = normalize(UnsafeServer),
	case Action of
		"query" ->
			Value = case Server of
				"@all" ->
					Actions = ets:foldl(fun(V, A) ->
								case V of
									{local_config, {{s2s_host, ListServer}, VHost}, ListAction} ->
										[{struct, [
													{server, ListServer},
													{action, to_string(ListAction)}
												]} | A];
									_ ->
										A
								end
						end, [], local_config),
					{array, Actions};
				_ ->
					to_string(ejabberd_config:get_local_option(s2s_filter_option_name(Server, VHost)))
			end,
			{false, {response, [Value]}};
		"allow" ->
			ejabberd_config:add_local_option(s2s_filter_option_name(Server, VHost), allow),
			{false, {response, [1]}};
		"deny" ->
			ejabberd_config:add_local_option(s2s_filter_option_name(Server, VHost), deny),
			{false, {response, [1]}};
		"delete" ->
			ets:delete(local_config, s2s_filter_option_name(Server, VHost)),
			{false, {response, [1]}};
		_ ->
			{false, {response, [0]}}
	end;

handler(_State, auth, Payload) ->
	?WARNING_MSG("unknown call: ~p~n", [Payload]),
	{false, {response, ["Unknown call"]}}.

room_state(Pid) ->
	{ok, R} = gen_fsm:sync_send_all_state_event(Pid, get_state),
	R.

room_config(Pid) ->
	{ok, R} = gen_fsm:sync_send_all_state_event(Pid, get_config),
	R.

room_brief_struct(Roomname, Host, State) ->
	NumParticipants = length(dict:fetch_keys(State#state.users)),
    History = (State#state.history)#lqueue.queue,
    LastMessageAt =
    case queue:is_empty(History) of
        true ->
        	"N/A";
        false ->
        	LastMessage = queue:last(History),
        	{_, _, _, LastMessageTime, _} = LastMessage,
        	jlib:timestamp_to_iso(LastMessageTime)
    end,
    {struct, [
    		{room, Roomname},
    		{host, Host},
    		{num_participants, NumParticipants},
    		{last_message_at, LastMessageAt},
    		{subject, State#state.subject},
    		{subject_author, State#state.subject_author}
    	]}.

room_config_struct(Config) ->
	{struct, [
			{title, Config#config.title},
			{description, Config#config.description},
			{allow_change_subj, Config#config.allow_change_subj},
			{allow_query_users, Config#config.allow_query_users},
			{allow_private_messages, Config#config.allow_private_messages},
			{allow_private_messages_from_visitors, erlang:atom_to_list(Config#config.allow_private_messages_from_visitors)},
			{allow_visitor_status, Config#config.allow_visitor_status},
			{allow_visitor_nickchange, Config#config.allow_visitor_nickchange},
			{public, Config#config.public},
			{public_list, Config#config.public_list},
			{persistent, Config#config.persistent},
			{moderated, Config#config.moderated},
			{captcha_protected, Config#config.captcha_protected},
			{members_by_default, Config#config.members_by_default},
			{members_only, Config#config.members_only},
			{allow_user_invites, Config#config.allow_user_invites},
			{password_protected, Config#config.password_protected},
			{password, Config#config.password},
			{anonymous, Config#config.anonymous},
			{allow_voice_requests, Config#config.allow_voice_requests},
			{voice_request_min_interval, Config#config.voice_request_min_interval},
			{max_users, Config#config.max_users},
			{logging, Config#config.logging},
			{captcha_whitelist, {array, lists:map(
						fun jlib:jid_to_string/1,
						?SETS:to_list(Config#config.captcha_whitelist))}},
			{filter_jid,
				case Config#config.filter_jid of
					undefined ->
						"";
					Value ->
						jlib:jid_to_string(Value)
				end},
		    {owner_jid,
		        case Config#config.owner_jid of
		        	undefined ->
		        		"";
		        	"" ->
		        		"";
		            Value ->
		            	jlib:jid_to_string(Value)
		        end}]}.

room_detailed_struct(Roomname, Host, State, Config) ->
	Affiliations = ?DICT:to_list(State#state.affiliations),
	AffiliationsArray = {array,
			lists:map(
			    fun({{Uname, Domain, _Res}, {Aff, Reason}}) when is_atom(Aff)->
			        	{struct, [{user, Uname}, {domain, Domain}, {affiliation, atom_to_list(Aff)}, {reason, Reason}]};
			        ({{Uname, Domain, _Res}, Aff}) when is_atom(Aff)->
			        	{struct, [{user, Uname}, {domain, Domain}, {affiliation, atom_to_list(Aff)}, {reason, ""}]} 
			    end, Affiliations)},
	UsersArray = {array,
		lists:map(
			fun({_JID, #user{jid = JID, nick = Nick, role = Role}}) ->
						{struct, [
								{jid, jlib:jid_to_string(JID)},
								{nick, Nick},
								{role, erlang:atom_to_list(Role)}
							]}
				end, ?DICT:to_list(State#state.users))},
	ConfigStruct = room_config_struct(Config),
	{struct, Values} = room_brief_struct(Roomname, Host, State),
    {struct, Values ++ [
    		{affiliations, AffiliationsArray},
    		{config, ConfigStruct},
    		{users, UsersArray}
    	]}.

s2s_filter_option_name(Server, VHost) ->
	case Server of
		"@default" ->
			{s2s_default_policy, VHost};
		_ ->
			{{s2s_host, Server}, VHost}
	end.

s2s_infos(Dir) ->
	case Dir of
		"in" ->
			ejabberd_s2s:get_info_s2s_connections(in);
		"out" ->
			ejabberd_s2s:get_info_s2s_connections(out);
		_ ->
			lists:append(
				ejabberd_s2s:get_info_s2s_connections(in),
				ejabberd_s2s:get_info_s2s_connections(out)
			)
	end.

normalize(String) ->
	erlang:binary_to_list(unicode:characters_to_binary(String)).

to_string(Value) ->
	unicode:characters_to_list(io_lib:format("~s", [Value])).
