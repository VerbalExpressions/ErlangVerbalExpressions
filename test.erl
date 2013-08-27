-module(test).

-compile(export_all).

%% Basic tests
%%

test_range() ->
    VE = vberl:range(vberl:startOfLine(vberl:new()), [$a, $c, $X, $Z]),

    %% true = vberl:match(VE, "a"),
    %% true = vberl:match(VE, "b"),
    %% true = vberl:match(VE, "c"),
    %% false = vberl:match(VE, "d"),
    %% true = vberl:match(VE, "X"),
    %% true = vberl:match(VE, "Y"),
    %% true = vberl:match(VE, "Z"),
    %% false = vberl:match(VE, "W").

    Tester = fun (TestCase) ->
		     lists:map(fun (Char) -> vberl:match(VE, [Char]) end,
			       TestCase)
	     end,
    [true, true, true, true, true, true] = Tester([$a, $b, $c, $X, $Y, $Z]),
    [false, false] = Tester([$d, $W]).

test_startOfLine() ->
    VE = vberl:startOfLine(vberl:find(vberl:new(), "HEAD")),
    false = vberl:match(VE, "text"),
    false = vberl:match(VE, "HEAtext"),
    true = vberl:match(VE, "HEADtext"),

    VE1 = vberl:withAnyCase(VE),
    false = vberl:match(VE1, "text"),
    false = vberl:match(VE, "HeAtext"),
    true = vberl:match(VE1, "hEaDtext").

test_endOfLine() ->
    VE = vberl:endOfLine(vberl:find(vberl:new(), "TAIL")),
    false = vberl:match(VE, "text"),
    true = vberl:match(VE, "textTAIL"),

    VE1 = vberl:withAnyCase(VE),
    false = vberl:match(VE1, "text"),
    true = vberl:match(VE1, "textTaIL").

test_anything() ->
    VE = vberl:endOfLine(vberl:anything(vberl:startOfLine(vberl:new()))),
    true = vberl:match(VE, "!#%&/()=?`-,'*|"),
    true = vberl:match(VE, "").

test_anythingIn() ->
    VE = vberl:endOfLine(vberl:anythingIn(vberl:startOfLine(vberl:new()), "!#%&/()=?`-,'*|")),
    true = vberl:match(VE, "!#%&/()=?`-,'*|%!?()"),
    
    VE2 = vberl:endOfLine(vberl:anythingIn(vberl:startOfLine(vberl:new()), "ABC")),
    true = vberl:match(VE2, "ABC"),
    false = vberl:match(VE2, " ABC"),
    false = vberl:match(VE2, "aBC"),
    false = vberl:match(VE2, "ABCx"),
    true = vberl:match(VE2, ""),
    
    VE3 = vberl:endOfLine(vberl:anythingIn(vberl:new(), "ABC")),
    true = vberl:match(VE3, "ABC"),
    true = vberl:match(VE3, " A  BC"),
    true = vberl:match(VE3, "aBC"),
    true = vberl:match(VE3, "ABCx"),
    true = vberl:match(VE3, "").

test_anythingBut() ->
    VE = vberl:endOfLine(vberl:anythingBut(vberl:startOfLine(vberl:new()), "ERL")),
    false = vberl:match(VE, "HELLO"),
    true = vberl:match(VE, "H***O").

test_something() ->
    VE = vberl:endOfLine(vberl:something(vberl:startOfLine(vberl:new()))),
    true = vberl:match(VE, "!#%&/()=?`-,'*|"),
    false = vberl:match(VE, ""). % at leat one character

test_find() ->
    VE = vberl:find(vberl:new(), "world"),
    false = vberl:match(VE, "hello wrld!"),
    true = vberl:match(VE, "hello world!"),

    VE1 = vberl:startOfLine(VE),
    false = vberl:match(VE1, "hello wrld!"), % still false
    false = vberl:match(VE1, "hello world!"), % not true anymore :)
    true = vberl:match(VE1, "world!").

test_maybe() ->
    VE = vberl:then(vberl:maybe(vberl:somethingIn(vberl:find(vberl:new(), "hello"), [$\s, $\t]), "world"), "!"),
    false = vberl:match(VE, "hello wrld!"), % nothing but "world"
    false = vberl:match(VE, "hello world !"), % no space allowed after "world", if present
    true = vberl:match(VE, "hello !"),
    false = vberl:match(VE, "hello!"), % at least one space after "hello" because of somethingIn !
    true = vberl:match(VE, "hello     world!").

test_alt() ->
    VE = vberl:find(vberl:alt(vberl:find(vberl:anything(vberl:startOfLine(vberl:new())), "G")), "h"),
    true = vberl:match(VE, "Github"),
    true = vberl:match(VE, " Github"),
    true = vberl:match(VE, "Git*ub pad"),
    true = vberl:match(VE, "*ithub pad"),
    false = vberl:match(VE, "*it*ub pad"),
    false = vberl:match(VE, "Bitbucket").

test_multiline() ->
    VE = vberl:searchMultiline(vberl:anything(vberl:find(vberl:anything(vberl:startOfLine(vberl:new())), "Pong"))),
    true = vberl:match(VE, "Ping \n Pong \n Ping"),

    VE2 = vberl:anything(vberl:find(vberl:anything(vberl:startOfLine(vberl:new())), "Pong")),
    false = vberl:match(VE2, "Ping \n Pong \n Ping"),
    true = vberl:match(VE2, "Pong \n Pong \n Ping").

test_case_sensitive() ->
    VE = vberl:endOfLine(vberl:find(vberl:startOfLine(vberl:new()),
				    "THOR")),
    false = vberl:match(VE, "thor").

test_case_insensitive() ->
    VE = vberl:withAnyCase(vberl:endOfLine(vberl:find(vberl:startOfLine(vberl:new()),
						      "THOR"))),
    true = vberl:match(VE, "thor").

test_lineBreak() ->
    VE = vberl:endOfLine(vberl:something(vberl:lineBreak(vberl:something(vberl:startOfLine(vberl:new()))))),
    true = vberl:match(VE, "foo\nbar"),
    false = vberl:match(VE, "foo\rbar"),
    false = vberl:match(VE, "foo bar").

test_tab() ->
    VE = vberl:endOfLine(vberl:something(vberl:tab(vberl:something(vberl:startOfLine(vberl:new()))))),
    true = vberl:match(VE, "foo\tbar"),
    false = vberl:match(VE, "foo\rbar"),
    false = vberl:match(VE, "foo bar").

test_word() ->
    VE = vberl:endOfLine(vberl:word(vberl:startOfLine(vberl:new()))),
    true = vberl:match(VE, "abcdefghijklmnopqrstuvwxyz0123456789_"),
    true = vberl:match(VE, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    true = vberl:match(VE, "a_c"),
    false = vberl:match(VE, "a-c"),
    false = vberl:match(VE, "a c"),
    false = vberl:match(VE, "a!c").

test_replace() ->
    VE1 = vberl:find(vberl:new(), "red"),
    "We have a blue house" = vberl:replace(VE1, "We have a red house", "blue"),

    VE2 = vberl:find(vberl:alt(VE1), "blue"),
    true = "We have a cyan house, a green car and a red bull" == vberl:replace(VE2, "We have a cyan house, a blue car and a red bull", "green"), % replace "blue"
    false = "We have a cyan house, a green car and a green bull" == vberl:replace(VE2, "We have a cyan house, a blue car and a red bull", "green"), % does not replace "red"

    VE3 = vberl:searchGlobal(VE2),
    true = "We have a cyan house, a green car and a green bull" == vberl:replace(VE3, "We have a cyan house, a blue car and a red bull", "green"), % replace both

    VE4 = vberl:withAnyCase(VE3),
    true = "We have a cyan house, a green car and a green bull" == vberl:replace(VE4, "We have a cyan house, a bLuE car and a rEd bull", "green").

%% Advanced tests
%%

test_match_url() ->
    VE0 = vberl:new(),
    VE1 = vberl:startOfLine(VE0),
    VE2 = vberl:then(VE1, "http"),
    VE3 = vberl:maybe(VE2, "s"),
    VE4 = vberl:then(VE3, "://"),
    VE5 = vberl:maybe(VE4, "www."),
    VE6 = vberl:word(VE5),
    VE7 = vberl:then(VE6, "."),
    VE8 = vberl:word(VE7),
    VE9 = vberl:maybe(VE8, "/"),
    VE10 = vberl:endOfLine(VE9),
    
    false = vberl:match(VE10, "www.google.com"),
    true = vberl:match(VE10, "http://www.google.com"),
    true = vberl:match(VE10, "https://www.google.com"),
    false = vberl:match(VE10, "httpx://www.google.com"),
    true = vberl:match(VE10, "https://www.google.com/").

%% Test runner
%% 

test() ->
    [{exports, Exports} | _] = ?MODULE:module_info(),
    Pred = fun([$t, $e, $s, $t, $_ | _]) -> true;
	      (_) -> false
	   end,
    [apply(?MODULE, run_test, [Export]) || {Export, _Arity} <- Exports, Pred(atom_to_list(Export))].

run_test(Func) ->
    io:format("Test ~p: ", [Func]),
    try
	apply(?MODULE, Func, []),
	io:format("PASS~n")
    of
	_ ->
	    ok
    catch
	X:Y ->
	    [{_M, _F, _A, [{file, _Filename}, {line, Line}]} | _Tail] = erlang:get_stacktrace(),
	    io:format("FAILED [with error ~p:~p at line ~p]~n", [X, Y, Line]),
	    error
    end.

