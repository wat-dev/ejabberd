%%%----------------------------------------------------------------------
%%% File    : mod_caps_filter.erl
%%% Author  : None
%%%----------------------------------------------------------------------

-module(mod_filter).
-author('no@mail.yet').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% hook handlers
-export([user_send_packet/3, filter_packet/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_filter).

start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 75),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 75),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 75),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 75),
    ok.

utf8_length(Data) ->
	case catch erlang:list_to_binary(Data) of
		Bin when is_binary(Bin) ->
			case catch unicode:characters_to_list(Bin, utf8) of
				List when is_list(List) ->
					length(List);
				_ ->
					length(Data)
			end;
		_ ->
			length(Data)
	end.

%%    			case ejabberd_sm:get_session_pid(From#jid.user, From#jid.server, From#jid.resource) of
%%    				none ->
%%    					ok;
%%    				Pid ->
%%    					Pid ! replaced,
%%    					ok
%%    			end;

user_send_packet(_From, _To, _Packet) ->
	ok.

bad_words() ->
	{ok, R} = re:compile("goo\.gl|rb2\.in|wed\.sy|v\.ht|syriat..?k\.[a-z]{2,3}|matrix[a-z]*\.[a-z]{2,3}|syriaroom\.[a-z]{2,3}|syriabuzz\.org|emoje\.[a-z]{2,3}|jsmart.web.id|talko\.ml", [caseless]),
	[{R, "syriatalk.biz"}].
filter_out_word({BadWord, GoodWord}, String) ->
    re:replace(String, BadWord, GoodWord, [global, {return, list}]).
filter_string(String) ->
    BadWords = bad_words(),
    lists:foldl(fun filter_out_word/2, String, BadWords).

filter_message([{xmlelement, Name, Attrs, Els} | Tail]) ->
    NewEls =
	case Name of
	    "subject" ->
		[{xmlcdata, filter_string(xml:get_cdata(Els))}];
	    "body" ->
		[{xmlcdata, filter_string(xml:get_cdata(Els))}];
		"status" ->
		[{xmlcdata, filter_string(xml:get_cdata(Els))}];
	    _ ->
		Els
	end,
    [{xmlelement, Name, Attrs, NewEls} | filter_message(Tail)];
filter_message([_ | Tail]) ->
    filter_message(Tail);
filter_message([]) ->
    [].

filter_packet({From, To, {xmlelement, "message", Attrs, Els} = Packet}) ->
	Body = xml:get_subtag_cdata(Packet, "body"),
	case utf8_length(Body) > 700 of
		true ->
			drop;
		_ ->
			{From, To, {xmlelement, "message", Attrs, filter_message(Els)}}
	end;
filter_packet({From, To, {xmlelement, "presence", Attrs, Els}}) ->
	{From, To, {xmlelement, "presence", Attrs, filter_message(Els)}};
filter_packet(Arg) ->
	Arg.
