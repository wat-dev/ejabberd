%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

%%% Implements most of XEP-0133: Service Administration Version 1.1
%%% (2005-08-19)

-module(mod_configure).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 get_local_identity/5,
	 get_local_features/5,
	 get_local_items/5,
	 adhoc_local_items/4,
	 adhoc_local_commands/4,
	 get_sm_identity/5,
	 get_sm_features/5,
	 get_sm_items/5,
	 adhoc_sm_items/4,
	 adhoc_sm_commands/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-define(T(Lang, Text), translate:translate(Lang, Text)).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

start(Host, _Opts) ->
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_items, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:add(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:add(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ejabberd_hooks:delete(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_local_items, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_COMMANDS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_COMMANDS).

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[{xmlelement, "identity",
	  [{"category", Category},
	   {"type", Type},
	   {"name", ?T(Lang, Name)}], []}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY("automation", "command-node", Name, Lang)).

-define(NODEJID(To, Name, Node),
	{xmlelement, "item",
	 [{"jid", jlib:jid_to_string(To)},
	  {"name", ?T(Lang, Name)},
	  {"node", Node}], []}).

-define(NODE(Name, Node),
	{xmlelement, "item",
	 [{"jid", Server},
	  {"name", ?T(Lang, Name)},
	  {"node", Node}], []}).

-define(NS_ADMINX(Sub), ?NS_ADMIN++"#"++Sub).
-define(NS_ADMINL(Sub), ["http:","jabber.org","protocol","admin", Sub]).
tokenize(Node) -> string:tokens(Node, "/#").

get_sm_identity(Acc, _From, _To, Node, Lang) ->
    case Node of
	"config" ->
	    ?INFO_COMMAND("Configuration", Lang);
	_ ->
	    Acc
    end.

get_local_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(Node),
    case LNode of
	?NS_ADMINL("add-user") ->
	    ?INFO_COMMAND("Add User", Lang);
	?NS_ADMINL("delete-user") ->
	    ?INFO_COMMAND("Delete User", Lang);
	?NS_ADMINL("end-user-session") ->
	    ?INFO_COMMAND("End User Session", Lang);
	?NS_ADMINL("change-user-password") ->
	    ?INFO_COMMAND("Change User Password", Lang);
	?NS_ADMINL("get-user-lastlogin") ->
	    ?INFO_COMMAND("Get User Last Login Time", Lang);
	?NS_ADMINL("user-stats") ->
	    ?INFO_COMMAND("Get User Statistics", Lang);
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats),
	case Allow of
	    deny ->
		{error, ?ERR_FORBIDDEN};
	    allow ->
		{result, Feats}
	end).

get_sm_features(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Allow = acl:match_rule(LServer, configure, From),
	    case Node of
		"config" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

get_local_features(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = tokenize(Node),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		?NS_ADMINL("add-user") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMINL("delete-user") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMINL("end-user-session") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMINL("change-user-password") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMINL("get-user-lastlogin") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMINL("user-stats") ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

adhoc_sm_items(Acc, From, #jid{lserver = LServer} = To, Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Nodes = [{xmlelement, "item",
		      [{"jid", jlib:jid_to_string(To)},
		       {"name", ?T(Lang, "Configuration")},
		       {"node", "config"}], []}],
	    {result, Items ++ Nodes};
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

get_sm_items(Acc, From,
	     #jid{user = User, server = Server, lserver = LServer} = To,
	     Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    case {acl:match_rule(LServer, configure, From), Node} of
		{allow, ""} ->
		    Nodes = [?NODEJID(To, "Configuration", "config"),
			     ?NODEJID(To, "User Management", "user")],
		    {result, Items ++ Nodes ++ get_user_resources(User, Server)};
		{allow, "config"} ->
		    {result, []};
		{_, "config"} ->
		    {error, ?ERR_FORBIDDEN};
		_ ->
		    Acc
	    end
    end.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ Server ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).

%%%-----------------------------------------------------------------------

adhoc_local_items(Acc, From, #jid{lserver = LServer, server = Server} = To,
		  Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    PermLev = get_permission_level(From),
	    %% Recursively get all configure commands
	    Nodes = recursively_get_local_items(PermLev, LServer, "", Server,
						Lang),
	    Nodes1 = lists:filter(
		       fun(N) ->
			       Nd = xml:get_tag_attr_s("node", N),
			       F = get_local_features([], From, To, Nd, Lang),
			       case F of
				   {result, [?NS_COMMANDS]} ->
				       true;
				   _ ->
				       false
			       end
		       end, Nodes),
	    {result, Items ++ Nodes1};
	_ ->
	    Acc
    end.

recursively_get_local_items(PermLev, LServer, Node, Server, Lang) ->
    LNode = tokenize(Node),
    Items = case get_local_items({PermLev, LServer}, LNode, Server, Lang) of
		{result, Res} ->
		    Res;
		{error, _Error} ->
		    []
	    end,
    Nodes = lists:flatten(
	      lists:map(
		fun(N) ->
			S = xml:get_tag_attr_s("jid", N),
			Nd = xml:get_tag_attr_s("node", N),
			if (S /= Server) or (Nd == "") ->
				[];
			   true ->
				[N, recursively_get_local_items(
				      PermLev, LServer, Nd, Server, Lang)]
			end
		end, Items)),
    Nodes.

get_permission_level(JID) ->
    case acl:match_rule(global, configure, JID) of
	allow -> global;
	deny -> vhost
    end.

%%%-----------------------------------------------------------------------

-define(ITEMS_RESULT(Allow, LNode, Fallback),
	case Allow of
	    deny ->
		Fallback;
	    allow ->
		PermLev = get_permission_level(From),
		case get_local_items({PermLev, LServer}, LNode,
				     jlib:jid_to_string(To), Lang) of
		    {result, Res} ->
			{result, Res};
		    {error, Error} ->
			{error, Error}
		end
	end).

get_local_items(Acc, From, #jid{lserver = LServer} = To, "", Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Allow = acl:match_rule(LServer, configure, From),
	    case Allow of
		deny ->
		    {result, Items};
		allow ->
		    PermLev = get_permission_level(From),
		    case get_local_items({PermLev, LServer}, [],
					 jlib:jid_to_string(To), Lang) of
			{result, Res} ->
			    {result, Items ++ Res};
			{error, _Error} ->
			    {result, Items}
		    end
	    end
    end;

get_local_items(Acc, From, #jid{lserver = LServer} = To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = tokenize(Node),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		?NS_ADMINL("add-user") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		?NS_ADMINL("delete-user") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		?NS_ADMINL("end-user-session") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		?NS_ADMINL("change-user-password") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		?NS_ADMINL("get-user-lastlogin") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		?NS_ADMINL("user-stats") ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

%% @spec ({PermissionLevel, Host}, [string()], Server::string(), Lang)
%%              -> {result, [xmlelement()]}
%%       PermissionLevel = global | vhost
get_local_items(_Host, [], Server, Lang) ->
    {result,
     [?NODE("Add User",            ?NS_ADMINX("add-user")),
      ?NODE("Delete User",         ?NS_ADMINX("delete-user")),
      ?NODE("End User Session",    ?NS_ADMINX("end-user-session")),
      ?NODE("Change User Password",?NS_ADMINX("change-user-password")),
      ?NODE("Get User Last Login Time",   ?NS_ADMINX("get-user-lastlogin")),
      ?NODE("Get User Statistics",   ?NS_ADMINX("user-stats"))
     ]};

get_local_items(_Host, ["http:" | _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, _, _Server, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.

%%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(LServerOrGlobal, From, To, Request),
	case acl:match_rule(LServerOrGlobal, configure, From) of
	    deny ->
		{error, ?ERR_FORBIDDEN};
	    allow ->
		adhoc_local_commands(From, To, Request)
	end).

adhoc_local_commands(Acc, From, #jid{lserver = LServer} = To,
		     #adhoc_request{node = Node} = Request) ->
    LNode = tokenize(Node),
    case LNode of
	?NS_ADMINL(_) ->
	    ?COMMANDS_RESULT(LServer, From, To, Request);
	_ ->
	    Acc
    end.

adhoc_local_commands(From, #jid{lserver = LServer} = _To,
		     #adhoc_request{lang = Lang,
				    node = Node,
				    sessionid = SessionID,
				    action = Action,
				    xdata = XData} = Request) ->
    LNode = tokenize(Node),
    %% If the "action" attribute is not present, it is
    %% understood as "execute".  If there was no <actions/>
    %% element in the first response (which there isn't in our
    %% case), "execute" and "complete" are equivalent.
    ActionIsExecute = lists:member(Action,
				   ["", "execute", "complete"]),
    if	Action == "cancel" ->
	    %% User cancels request
	    adhoc:produce_response(
	      Request, 
	      #adhoc_response{status = canceled});
	XData == false, ActionIsExecute ->
	    %% User requests form
	    case get_form(LServer, LNode, Lang) of
		{result, Form} ->
		    adhoc:produce_response(
		      Request,
		      #adhoc_response{status = executing,
				      elements = Form});
		{result, Status, Form} ->
		    adhoc:produce_response(
		      Request,
		      #adhoc_response{status = Status,
				      elements = Form});
		{error, Error} ->
		    {error, Error}
	    end;
	XData /= false, ActionIsExecute ->
	    %% User returns form.
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		Fields ->
		    case catch set_form(From, LServer, LNode, Lang, Fields) of
			{result, Res} ->
				odbc_queries:add_security_log(LServer,
					ejabberd_odbc:escape(From#jid.user),
					ejabberd_odbc:escape(From#jid.server),
					"mod_configure",
					"submit_form",
					io_lib:format("~p: ~p", [LNode, Fields]),
					now()),
			    adhoc:produce_response(
			      #adhoc_response{lang = Lang,
			                      node = Node,
					      sessionid = SessionID,
					      elements = Res,
					      status = completed});
			{'EXIT', _} ->
			    {error, ?ERR_BAD_REQUEST};
			{error, Error} ->
			    {error, Error}
		    end
	    end;
	true ->
	    {error, ?ERR_BAD_REQUEST}
    end.


-define(TVFIELD(Type, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).
-define(HFIELD(), ?TVFIELD("hidden", "FORM_TYPE", ?NS_ADMIN)).

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", ?T(Lang, Label)},
			       {"var", Var}], []}).

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", ?T(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(XMFIELD(Type, Label, Var, Vals),
	{xmlelement, "field", [{"type", Type},
			       {"label", ?T(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata,Val}]} || Val <- Vals]}).

-define(TABLEFIELD(Table, Val),
	{xmlelement, "field", [{"type", "list-single"},
			       {"label", atom_to_list(Table)},
			       {"var", atom_to_list(Table)}],
	 [{xmlelement, "value", [], [{xmlcdata, atom_to_list(Val)}]},
	  {xmlelement, "option", [{"label",
				   ?T(Lang, "RAM copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "ram_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   ?T(Lang,
				      "RAM and disc copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   ?T(Lang,
				      "Disc only copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_only_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   ?T(Lang, "Remote copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "unknown"}]}]}
	 ]}).

get_form(_Host, ?NS_ADMINL("add-user"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "Add User")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-single"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjid"}],
		 [{xmlelement, "required", [], []}]},
	        {xmlelement, "field", 
		 [{"type", "text-private"},
		  {"label", ?T(Lang, "Password")},
		  {"var", "password"}],
		 [{xmlelement, "required", [], []}]},
	        {xmlelement, "field", 
		 [{"type", "text-private"},
		  {"label", ?T(Lang, "Password Verification")},
		  {"var", "password-verify"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("delete-user"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "Delete User")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-multi"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjids"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("end-user-session"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "End User Session")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-single"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjid"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("change-user-password"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "Get User Password")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-single"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjid"}],
		 [{xmlelement, "required", [], []}]},
	        {xmlelement, "field", 
		 [{"type", "text-private"},
		  {"label", ?T(Lang, "Password")},
		  {"var", "password"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("get-user-lastlogin"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "Get User Last Login Time")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-single"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjid"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, ?NS_ADMINL("user-stats"), Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
		 [{xmlcdata, ?T(Lang, "Get User Statistics")}]},
	        {xmlelement, "field", 
		 [{"type", "jid-single"},
		  {"label", ?T(Lang, "Jabber ID")},
		  {"var", "accountjid"}],
		 [{xmlelement, "required", [], []}]}
	       ]}]};

get_form(_Host, _, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

set_form(From, Host, ?NS_ADMINL("add-user"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    Password = get_value("password", XData),
    Password = get_value("password-verify", XData),
    AccountJID = jlib:string_to_jid(AccountString),
    User = AccountJID#jid.luser,
    Server = AccountJID#jid.lserver,
    true = lists:member(Server, ?MYHOSTS),
    true = (Server == Host) orelse (get_permission_level(From) == global),
    ejabberd_auth:try_register(User, Server, Password, "admin-console"),
    {result, []};

set_form(From, Host, ?NS_ADMINL("delete-user"), _Lang, XData) ->
    AccountStringList = get_values("accountjids", XData),
    [_|_] = AccountStringList,
    ASL2 = lists:map(
	     fun(AccountString) ->
		     JID = jlib:string_to_jid(AccountString),
		     [_|_] = JID#jid.luser,
		     User = JID#jid.luser, 
		     Server = JID#jid.lserver,
		     true = (Server == Host) orelse (get_permission_level(From) == global),
		     true = ejabberd_auth:is_user_exists(User, Server),
		     {User, Server}
	     end,
	     AccountStringList),
    [ejabberd_auth:remove_user(User, Server) || {User, Server} <- ASL2],
    {result, []};

set_form(From, Host, ?NS_ADMINL("end-user-session"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = jlib:string_to_jid(AccountString),
    [_|_] = JID#jid.luser,
    LUser = JID#jid.luser, 
    LServer = JID#jid.lserver, 
    true = (LServer == Host) orelse (get_permission_level(From) == global),
    %% Code copied from ejabberd_sm.erl
    case JID#jid.lresource of
	[] -> 
	    SIDs = mnesia:dirty_select(session,
				       [{#session{sid = '$1', usr = {LUser, LServer, '_'}, _ = '_'}, [], ['$1']}]),
	    [Pid ! replaced || {_, Pid} <- SIDs];
	R -> 
	    [{_, Pid}] = mnesia:dirty_select(session,
					     [{#session{sid = '$1', usr = {LUser, LServer, R}, _ = '_'}, [], ['$1']}]),
	    Pid ! replaced
    end, 
    {result, []};

set_form(From, Host, ?NS_ADMINL("change-user-password"), _Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    Password = get_value("password", XData),
    JID = jlib:string_to_jid(AccountString),
    [_|_] = JID#jid.luser,
    User = JID#jid.luser, 
    Server = JID#jid.lserver, 
    true = (Server == Host) orelse (get_permission_level(From) == global),
    true = ejabberd_auth:is_user_exists(User, Server),
    true = (get_permission_level(From) == global) orelse (get_permission_level(JID) /= global),
    ejabberd_auth:set_password(User, Server, Password),
    {result, []};

set_form(From, Host, ?NS_ADMINL("get-user-lastlogin"), Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = jlib:string_to_jid(AccountString),
    [_|_] = JID#jid.luser,
    User = JID#jid.luser, 
    Server = JID#jid.lserver, 
    true = (Server == Host) orelse (get_permission_level(From) == global),

    %% Code copied from web/ejabberd_web_admin.erl
    %% TODO: Update time format to XEP-0202: Entity Time
    FLast =
	case ejabberd_sm:get_user_resources(User, Server) of
	    [] ->
		_US = {User, Server},
		case get_last_info(User, Server) of
		    not_found ->
			?T(Lang, "Never");
		    {ok, Timestamp, _Status} ->
			Shift = Timestamp,
			TimeStamp = {Shift div 1000000,
				     Shift rem 1000000,
				     0},
			{{Year, Month, Day}, {Hour, Minute, Second}} =
			    calendar:now_to_local_time(TimeStamp),
			lists:flatten(
			  io_lib:format(
			    "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			    [Year, Month, Day, Hour, Minute, Second]))
		end;
	    _ ->
		?T(Lang, "Online")
	end,
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "result"}],
	       [?HFIELD(),
		?XFIELD("jid-single", "Jabber ID", "accountjid", AccountString),
	        ?XFIELD("text-single", "Last login", "lastlogin", FLast)
	       ]}]};

set_form(From, Host, ?NS_ADMINL("user-stats"), Lang, XData) ->
    AccountString = get_value("accountjid", XData),
    JID = jlib:string_to_jid(AccountString),
    [_|_] = JID#jid.luser,
    User = JID#jid.luser, 
    Server = JID#jid.lserver, 
    true = (Server == Host) orelse (get_permission_level(From) == global),

    Resources = ejabberd_sm:get_user_resources(User, Server),
    IPs1 = [ejabberd_sm:get_user_ip(User, Server, Resource) || Resource <- Resources],
    IPs = [inet_parse:ntoa(IP)++":"++integer_to_list(Port) || {IP, Port} <- IPs1],

    Items = ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]),
    Rostersize = integer_to_list(erlang:length(Items)),

    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		?XFIELD("jid-single", "Jabber ID", "accountjid", AccountString),
	        ?XFIELD("text-single", "Roster size", "rostersize", Rostersize),
	        ?XMFIELD("text-multi", "IP addresses", "ipaddresses", IPs),
	        ?XMFIELD("text-multi", "Resources", "onlineresources", Resources)
	       ]}]};

set_form(_From, _Host, _, _Lang, _XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

get_value(Field, XData) -> 
    hd(get_values(Field, XData)).
get_values(Field, XData) -> 
    {value, {_, ValueList}} = lists:keysearch(Field, 1, XData),
    ValueList.

get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
        true ->
            mod_last:get_last_info(User, Server);
        false ->
            not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adhoc_sm_commands(_Acc, From,
		  #jid{user = User, server = Server, lserver = LServer} = _To,
		  #adhoc_request{lang = Lang,
				 node = "config",
				 action = Action,
				 xdata = XData} = Request) ->
    case acl:match_rule(LServer, configure, From) of
	deny ->
	    {error, ?ERR_FORBIDDEN};
	allow ->
	    %% If the "action" attribute is not present, it is
	    %% understood as "execute".  If there was no <actions/>
	    %% element in the first response (which there isn't in our
	    %% case), "execute" and "complete" are equivalent.
	    ActionIsExecute = lists:member(Action,
					   ["", "execute", "complete"]),
	    if	Action == "cancel" ->
		    %% User cancels request
		    adhoc:produce_response(
		      Request, 
		      #adhoc_response{status = canceled});
		XData == false, ActionIsExecute ->
		    %% User requests form
		    case get_sm_form(User, Server, "config", Lang) of
			{result, Form} ->
			    adhoc:produce_response(
			      Request,
			      #adhoc_response{status = executing,
					      elements = Form});
			{error, Error} ->
			    {error, Error}
		    end;
		XData /= false, ActionIsExecute ->
		    %% User returns form.
		    case jlib:parse_xdata_submit(XData) of
			invalid ->
			    {error, ?ERR_BAD_REQUEST};
			Fields ->
			    set_sm_form(User, Server, "config", Request, Fields)
		    end;
		true ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;

adhoc_sm_commands(Acc, _From, _To, _Request) ->
    Acc.

get_sm_form(User, Server, "config", Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [?HFIELD(),
		{xmlelement, "title", [],
	         [{xmlcdata,
		   ?T(
		      Lang, "Administration of ") ++ User}]},
	        {xmlelement, "field",
	         [{"type", "list-single"},
		  {"label", ?T(Lang, "Action on user")},
		  {"var", "action"}],
	         [{xmlelement, "value", [], [{xmlcdata, "edit"}]},
		  {xmlelement, "option",
		   [{"label", ?T(Lang, "Edit Properties")}],
		   [{xmlelement, "value", [], [{xmlcdata, "edit"}]}]},
		  {xmlelement, "option",
		   [{"label", ?T(Lang, "Remove User")}],
		   [{xmlelement, "value", [], [{xmlcdata, "remove"}]}]}
	         ]},
	        ?XFIELD("text-private", "Password", "password",
		        ejabberd_auth:get_password_s(User, Server))
	       ]}]};

get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.


set_sm_form(User, Server, "config",
	    #adhoc_request{lang = Lang,
	                   node = Node,
			   sessionid = SessionID}, XData) ->
    Response = #adhoc_response{lang = Lang,
                               node = Node,
			       sessionid = SessionID,
			       status = completed},
    case lists:keysearch("action", 1, XData) of
	{value, {_, ["edit"]}} ->
	    case lists:keysearch("password", 1, XData) of
		{value, {_, [Password]}} ->
		    ejabberd_auth:set_password(User, Server, Password),
		    adhoc:produce_response(Response);
		_ ->
		    {error, ?ERR_NOT_ACCEPTABLE}
	    end;
	{value, {_, ["remove"]}} ->
	    catch ejabberd_auth:remove_user(User, Server),
	    adhoc:produce_response(Response);
	_ ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end;

set_sm_form(_User, _Server, _Node, _Request, _Fields) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.



