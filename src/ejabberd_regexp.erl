%%%----------------------------------------------------------------------
%%% File    : ejabberd_regexp.erl
%%% Author  : Badlop
%%% Purpose : Frontend to Re and Regexp OTP modules
%%% Created : 8 Dec 2011 by Badlop
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

-module(ejabberd_regexp).
-compile([export_all]).

exec(ReM, ReF, ReA, RgM, RgF, RgA) ->
    try apply(ReM, ReF, ReA)
    catch
	error:undef ->
	    apply(RgM, RgF, RgA);
	A:B ->
	    {error, {A, B}}
    end.

run(String, Regexp) ->
    case exec(re, run, [String, Regexp, [{capture, none}]], regexp, first_match, [String, Regexp]) of
	{match, _, _} -> match;
	{match, _} -> match;
	match -> match;
	nomatch -> nomatch;
	{error, Error} -> {error, Error}
    end.

split(String, Regexp) ->
    case exec(re, split, [String, Regexp, [{return, list}]], regexp, split, [String, Regexp]) of
	{ok, FieldList} -> FieldList;
	{error, Error} -> throw(Error);
	A -> A
    end.

replace(String, Regexp, New) ->
    case exec(re, replace, [String, Regexp, New, [{return, list}]], regexp, sub, [String, Regexp, New]) of
	{ok, NewString, _RepCount} -> NewString;
	{error, Error} -> throw(Error);
	A -> A
    end.

greplace(String, Regexp, New) ->
    case exec(re, replace, [String, Regexp, New, [global, {return, list}]], regexp, sub, [String, Regexp, New]) of
	{ok, NewString, _RepCount} -> NewString;
	{error, Error} -> throw(Error);
	A -> A
    end.

%%sh_to_awk(ShRegExp) ->
%%    case exec(xmerl_regexp, sh_to_awk, [ShRegExp], regexp, sh_to_awk, [ShRegExp]) of
%%	A -> A
%%    end.

sh_to_awk(Pattern) when is_list(Pattern) ->
    fnmatch_char(Pattern, [$(,$^]).

fnmatch_char([], Acc) ->
    lists:reverse([$$,$)|Acc]);
fnmatch_char([Ch|Tail], Acc) ->
    case Ch of
      $\\ ->
          fnmatch_char(Tail, [$\\,$\\|Acc]);
      $[ ->
          fnmatch_bexp_first(Tail, [Ch|Acc]);
      $? ->
          fnmatch_char(Tail, [$.|Acc]);
      $* ->
          fnmatch_char(Tail, [$*,$.|Acc]);
      _ ->
          case lists:member(Ch, "^.+{}()$|\\") of
              true ->
                  fnmatch_char(Tail, [Ch,$\\|Acc]);
              false ->
                  fnmatch_char(Tail, [Ch|Acc])
          end
    end.

fnmatch_bexp_first([], _) ->
    {error, unclosed_be};
fnmatch_bexp_first([Ch|Tail], Acc) ->
    case Ch of
      $] ->
          {error, empty_be};
      $! ->
          fnmatch_bexp_next(Tail, false, [$^|Acc]);
      $\\ ->
          fnmatch_bexp_next(Tail, true, Acc);
      _ ->
          fnmatch_bexp_next(Tail, false, [Ch|Acc])
    end.

fnmatch_bexp_next([], _, _) ->
    {error, unclosed_be};
fnmatch_bexp_next([Ch|Tail], true, Acc) ->
    fnmatch_bexp_next(Tail, false, [Ch,$\\|Acc]);
fnmatch_bexp_next([Ch|Tail], false, Acc) ->
    case Ch of
      $] ->
          fnmatch_char(Tail, [Ch|Acc]);
      $\\ ->
          fnmatch_bexp_next(Tail, true, Acc);
      _ ->
          fnmatch_bexp_next(Tail, false, [Ch|Acc])
    end.
