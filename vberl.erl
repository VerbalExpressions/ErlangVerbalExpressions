-module(vberl).

-export([new/0,
	 startOfLine/1,
	 startOfLine/2,
	 endOfLine/1,
	 endOfLine/2,
	 then/2,
	 find/2,
	 maybe/2,
	 anything/1,
	 anythingIn/2,
	 anythingBut/2,
	 something/1,
	 somethingIn/2,
	 somethingBut/2,
	 replace/3,
	 match/2,
	 lineBreak/1,
	 br/1,
	 tab/1,
	 word/1,
	 anyOf/2,
	 any/2,
	 range/2,
	 withAnyCase/1,
	 withAnyCase/2,
	 searchGlobal/1,
	 searchGlobal/2,
	 searchMultiline/1,
	 searchMultiline/2,
	 multiple/2,
	 alt/1, % "or" is a reserved keyword.
	 alt/2, % "or" is a reserved keyword.
	 beginCapture/1,
	 endCapture/1,
	 source/1,
	 regex/1
	]).

%%

-record(vexpr, {prefixes = "",
		suffixes = "",
		source = "",
		compile_modifiers = [],
		run_modifiers = [],
		re
	       }).

%%

new() ->
    #vexpr{}.

startOfLine(Expr) ->
    startOfLine(Expr, true).

startOfLine(Expr, true) ->
    add(Expr#vexpr{prefixes="^"}, "");
startOfLine(Expr, false) ->
    add(Expr#vexpr{prefixes=""}, "").

endOfLine(Expr) ->
    endOfLine(Expr, true).

endOfLine(Expr, true) ->
    add(Expr#vexpr{suffixes="\$"}, "");
endOfLine(Expr, false) ->
    add(Expr#vexpr{suffixes=""}, "").

then(Expr, Value) ->
    add(Expr, "(?:" ++ sanitize(Value) ++ ")").

find(Expr, Value) ->
    then(Expr, Value).

maybe(Expr, Value) ->
    add(Expr,  "(?:" ++ sanitize(Value) ++ ")?").

anything(Expr) ->
    add(Expr,  "(?:.*)").

anythingIn(Expr, Value) ->
    add(Expr,  "(?:[" ++ sanitize(Value) ++ "]*)").

anythingBut(Expr, Value) ->
    add(Expr,  "(?:[^" ++ sanitize(Value) ++ "]*)").

something(Expr) ->
    add(Expr,  "(?:.+)").

somethingIn(Expr, Value) ->
    add(Expr,  "(?:[" ++ sanitize(Value) ++ "]+)").

somethingBut(Expr, Value) ->
    add(Expr,  "(?:[^" ++ sanitize(Value) ++ "]+)").

replace(Expr, Source, Value) ->
    re:replace(Source, Expr#vexpr.re, Value, [{return, list} | Expr#vexpr.run_modifiers]).

match(Expr, Value) ->
    case re:run(Value, Expr#vexpr.re, Expr#vexpr.run_modifiers) of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.

lineBreak(Expr) ->
    add(Expr, "(?:(?:\\n)|(?:\\r\\n))").

br(Expr) ->
    lineBreak(Expr).

tab(Expr) ->
    add(Expr, "\\t").

word(Expr) ->
    add(Expr, "\\w+").

anyOf(Expr, Value) ->
    add(Expr, "[" ++ sanitize(Value) ++ "]").

any(Expr, Value) ->
    anyOf(Expr, Value).

%% range(Expr, [$a, $z, $A, $Z]).
range(Expr, Bounds) when is_list(Bounds), erlang:length(Bounds) rem 2 == 0 ->
    add(Expr, "[" ++ buildRange_(Bounds, "") ++ "]").

buildRange_([B, E | Params], Acc) ->
    buildRange_(Params, Acc ++ sanitize([B]) ++ "-" ++ sanitize([E]));
buildRange_([], Acc) ->
    Acc.

withAnyCase(Expr) ->
    modifier_(Expr, compile_modifiers, caseless, true).

withAnyCase(Expr, Flag) ->
    modifier_(Expr, compile_modifiers, caseless, Flag).

searchGlobal(Expr) -> %% same role as stopAtFirst, but reversed
    searchGlobal(Expr, true).

searchGlobal(Expr, Flag) -> %% same role as stopAtFirst, but reversed
    modifier_(Expr, run_modifiers, global, Flag).

searchMultiline(Expr) -> %% same role as searchOneLine, but reversed
    searchMultiline(Expr, true).

searchMultiline(Expr, Flag) -> %% same role as searchOneLine, but reversed
    modifier_(Expr, compile_modifiers, multiline, Flag).

modifier_(Expr, compile_modifiers, Modifier, true) ->
    M0 = Expr#vexpr.compile_modifiers,
    M1 = M0 -- [Modifier], % If modifier already present, remove it; if not, do nothing
    M2 = M1 ++ [Modifier], % Then, add it.
    add(Expr#vexpr{compile_modifiers = M2}, "");
modifier_(Expr, compile_modifiers, Modifier, false) ->
    add(Expr#vexpr{compile_modifiers = Expr#vexpr.compile_modifiers -- [Modifier]}, "");
modifier_(Expr, run_modifiers, Modifier, true) ->
    M0 = Expr#vexpr.run_modifiers,
    M1 = M0 -- [Modifier], % If modifier already present, remove it; if not, do nothing
    M2 = M1 ++ [Modifier], % Then, add it.
    add(Expr#vexpr{run_modifiers = M2}, "");
modifier_(Expr, run_modifiers, Modifier, false) ->
    add(Expr#vexpr{run_modifiers = Expr#vexpr.run_modifiers -- [Modifier]}, "").

multiple(Expr, Value = [C | _Tail]) when C == $*; C == $+ ->
    add(Expr, sanitize(Value));
multiple(Expr, Value) ->
    %% Javascript/Clojure and C++/Java differ on this point, the formers suffixing
    %% while the latters prefixing.
    %% Follow Javascript implementation.
    %% 
    add(Expr, sanitize(Value ++ "+")).

alt(Expr) ->
    %% Javascript/Clojure and C++/Java differ on this point, the formers always add parenthesis,
    %% the latters check the presence of parenthesis.
    %% Follow Javascript implementation.
    %% 
    E2 = Expr#vexpr{prefixes = Expr#vexpr.prefixes ++ "(?:",
		    suffixes = ")" ++ Expr#vexpr.suffixes},
    add(E2, ")|(?:").

alt(Expr, Value) ->
    then(alt(Expr), Value).

beginCapture(Expr) ->
    E2 = Expr#vexpr{suffixes = Expr#vexpr.suffixes ++ ")"},
    add(E2, "(").

endCapture(Expr) ->
    E2 = Expr#vexpr{suffixes = string:substr(Expr#vexpr.suffixes, 1, length(Expr#vexpr.suffixes)-1)},
    add(E2, ")").

source(Expr) ->
    Expr#vexpr.source.

regex(Expr) ->
    Expr#vexpr.re.

%%

sanitize(Value) ->
    re:replace(Value, "([-`=.$*+?^()\[\\]{}\\|])", "\\\\&", [{return, list}, global]).

add(Expr, Value) when is_list(Value) ->
    compile(Expr#vexpr{source = Expr#vexpr.source ++ Value}).

compile(Expr) ->
    {ok, RegExp} = re:compile(Expr#vexpr.prefixes ++ Expr#vexpr.source ++ Expr#vexpr.suffixes, Expr#vexpr.compile_modifiers),
    Expr#vexpr{re = RegExp}.

