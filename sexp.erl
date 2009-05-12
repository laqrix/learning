% Sample code to read something like R5RS s-expressions
-module(sexp).
-export([read/1, read_token/1, test/0]).

-define(WHITESPACE(C), (C == $ ) orelse (C == $\n) orelse (C == $\r) orelse (C == $\t)).
-define(DELIMITER(C), ?WHITESPACE(C) orelse (C == $() orelse (C == $)) orelse (C == $[) orelse (C == $]) orelse (C == $") orelse (C == $:)).
-define(DIGIT(C), (C >= $0) andalso (C =< $9)).
-define(LETTER(C), (C >= $A) andalso (C =< $Z) orelse 
                   (C >= $a) andalso (C =< $z)).
-define(SPECIAL_INITIAL(C), (C == $!) orelse (C == $$) orelse (C == $%) orelse (C == $&) orelse (C == $*) orelse (C == $/) orelse (C == $:) orelse (C == $<) orelse (C == $=) orelse (C == $>) orelse (C == $?) orelse (C == $^) orelse (C == $_) orelse (C == $~)).
-define(SPECIAL_SUBSEQUENT(C), (C == $+) orelse (C == $-) orelse (C == $.) orelse (C == $@)).

read(Input) -> datum(read_token(Input)).

datum({eof, []}) -> eof;
datum({Token, Rest}) ->
  case Token of
    {token, true} -> {true, Rest};
    {token, false} -> {false, Rest};
    {token, number, Data} -> {Data, Rest};
    {token, string, Data} -> {Data, Rest};
    {token, identifier, Data} -> {list_to_atom(Data), Rest};
    {token, open, Kind} -> build_list(Kind, false, Rest);
    {token, open_vector} -> 
      {VectorAsList, More} = build_vector(Rest),
      {list_to_tuple(VectorAsList), More};
    {token, Q} when (Q == quote) orelse (Q == quasiquote) orelse 
		    (Q == unquote) orelse (Q == unquote_splice) ->
      build_quote(Q, Rest);
    X -> exit({unexpected_token, X})
  end.

build_list(ParenKind, SeenFirst, Input) ->
  case read_token(Input) of
    {{token, close, ParenKind}, Rest} -> {[], Rest};
    {{token, close, _}, _} -> exit(invalid_close_paren);
    {{token, dot}, Rest} ->
      case SeenFirst of
	false -> exit(unexpected_dot);
	true ->
	  {Datum, More} = datum(read_token(Rest)),
	  case read_token(More) of
	    {{token, close, ParenKind}, More2} -> {Datum, More2};
	    _ -> exit(missing_close_paren)
	  end
      end;
    {Token, Rest} ->
      {Datum, More} = datum({Token, Rest}),
      {List, More2} = build_list(ParenKind, true, More),
      {[Datum | List], More2}
  end.

build_vector(Input) ->
  case read_token(Input) of
    {{token, close, round}, Rest} -> {[], Rest};
    {{token, close, square}, _} -> exit(invalid_close_paren);
    {{token, dot}, _} -> exit(unexpected_dot);
    {Token, Rest} ->
      {Datum, More} = datum({Token, Rest}),
      {Vector, More2} = build_vector(More),
      {[Datum | Vector], More2}
  end.

build_quote(Quoting, Input) ->
  case read_token(Input) of
    eof -> exit(unexpected_eof);
    {Token, Rest} -> 
      {Datum, More} = datum({Token, Rest}),
      {[Quoting,Datum],More}
  end.

read_token([]) -> {eof,[]};
read_token([C|Rest]) when ?WHITESPACE(C) -> read_token(Rest);
read_token([$;|Rest]) -> ignore_line(Rest);
read_token([$(|Rest]) -> {{token, open, round}, Rest};
read_token([$[|Rest]) -> {{token, open, square}, Rest};
read_token([$)|Rest]) -> {{token, close, round}, Rest};
read_token([$]|Rest]) -> {{token, close, square}, Rest};
read_token([$'|Rest]) -> {{token, quote}, Rest};
read_token([$`|Rest]) -> {{token, quasiquote}, Rest};
read_token([$#|Rest]) -> read_hash(Rest);
read_token([$,|Rest]) -> read_unquote(Rest);
read_token([$.|Rest]) -> read_dots(Rest);
read_token([$+|Rest]) ->
  case Rest of
    [C|_] when ?DIGIT(C) -> number_or_identifier(Rest, [$+]);
    [C|_] when ?DELIMITER(C) -> {{token, identifier, "+"}, Rest};
    [] -> {{token, identifier, "+"}, []};
    _ -> exit(bad_identifier)
  end;
read_token([$-|Rest]) ->
  case Rest of
    [C|_] when ?DIGIT(C) -> number_or_identifier(Rest, [$-]);
    [C|_] when ?DELIMITER(C) -> {{token, identifier, "-"}, Rest};
    [] -> {{token, identifier, "-"}, []};
    _ -> exit(bad_identifier)
  end;
read_token([$"|Rest]) -> read_string(Rest,[]);
read_token([C|Rest]) when ?DIGIT(C) -> number_or_identifier(Rest, [C]);
read_token([C|Rest]) -> read_identifier(Rest, [C]).

% Other States

ignore_line([]) -> read_token([]);
ignore_line([$\n|Rest]) -> read_token(Rest);
ignore_line([_C|Rest]) -> ignore_line(Rest).

% TODO read_hash should also handle special names like #\space #\newline
read_hash([]) -> exit(unexpected_eof);
read_hash([$(|Rest]) -> {{token, open_vector}, Rest};
read_hash([$t|Rest]) -> {{token, true}, Rest};
read_hash([$f|Rest]) -> {{token, false}, Rest};
read_hash([C|_]) -> exit({unexpected_hash_character, C}).

read_unquote([$@|Rest]) -> {{token, unquote_splice}, Rest};
read_unquote(Rest) -> {{token, unquote}, Rest}.

read_dots([]) -> {{token, dot}, []};
read_dots([C|Rest]) when ?DELIMITER(C) -> {{token, dot}, [C | Rest]};
read_dots([$.|Rest]) -> read_dots2(Rest).

read_dots2([]) -> exit({bad_identifier, ".."});
read_dots2([$.|Rest]) -> read_dots3(Rest).

read_dots3([]) -> {{token, triple_dots}, []};
read_dots3([C|Rest]) when ?DELIMITER(C) -> {{token, triple_dots}, [C | Rest]}.

number_or_identifier(Data) ->
  case catch list_to_integer(Data) of
    {'EXIT', {badarg, _}} ->
      case catch list_to_float(Data) of
	{'EXIT', {badarg, _}} ->
	  {token, identifier, Data};
	N ->
	  {token, number, N}
      end;
  N ->
    {token, number, N}
  end.

number_or_identifier([], Data) ->
  {number_or_identifier(lists:reverse(Data)), []};
number_or_identifier([C|Rest], Data) when ?DELIMITER(C) ->
  {number_or_identifier(lists:reverse(Data)), [C | Rest]};
number_or_identifier([C|Rest], Data) ->
  number_or_identifier(Rest, [C | Data]).

read_identifier([], Data) ->
  {{token, identifier, lists:reverse(Data)}, []};
read_identifier([C|Rest], Data) when ?DELIMITER(C) ->
  {{token, identifier, lists:reverse(Data)}, [C | Rest]};
read_identifier([C|Rest], Data) when ?LETTER(C) orelse ?SPECIAL_INITIAL(C) orelse ?DIGIT(C) orelse ?SPECIAL_SUBSEQUENT(C) ->
  read_identifier(Rest, [C | Data]);
read_identifier([C|_], _) ->
  exit({unexpected_character, C}).

read_string([$"|Rest],Data) -> {{token, string, lists:reverse(Data)}, Rest};
read_string([$\\|Rest],Data) -> read_escaped(Rest, Data);
read_string([C|Rest],Data) -> read_string(Rest, [C | Data]);
read_string([],Data) -> exit({unterminated_string, lists:reverse(Data)}).

read_escaped([$a|Rest],Data) -> read_string(Rest, [$a | Data]);
read_escaped([$b|Rest],Data) -> read_string(Rest, [$b | Data]);
read_escaped([$f|Rest],Data) -> read_string(Rest, [$f | Data]);
read_escaped([$n|Rest],Data) -> read_string(Rest, [$n | Data]);
read_escaped([$r|Rest],Data) -> read_string(Rest, [$r | Data]);
read_escaped([$t|Rest],Data) -> read_string(Rest, [$t | Data]);
read_escaped([$v|Rest],Data) -> read_string(Rest, [$v | Data]);
read_escaped([$\\|Rest],Data) -> read_string(Rest, [$\\ | Data]);
read_escaped([$"|Rest],Data) -> read_string(Rest, [$" | Data]);
read_escaped([$'|Rest],Data) -> read_string(Rest, [$' | Data]);
read_escaped([C|_Rest],_Data) -> exit({illegal_string_escape, C}).

% Testing
-define(EXPECT(Test, Reason), expect(catch Test, Reason)).

expect(Result, Expected) ->
  %io:format("expect(~w, ~w)~n", [Result, Expected]),
  case Result of
    {'EXIT', Expected} -> ok;
    Other -> exit({test_failed, Other, Expected})
  end.

test() ->
  % unhandled: +inf, -inf, +foo
  % unhandled: cases that involve a premature eof
  % incorrect: #true, #false

  eof = read(""),
  eof = read("  "),
  eof = read("; This is a comment"),
  {"", []} = read("\"\""),
  {"test", []} = read("\"test\""),
  ?EXPECT(read("\""), {unterminated_string, []}),
  ?EXPECT(read("\"test"), {unterminated_string, "test"}),

  {test, []} = read("test"),
  {[quote,test], []} = read("'test"),
  {[quasiquote,test], []} = read("`test"),
  {[unquote,test], []} = read(",test"),
  {[unquote_splice,test], []} = read(",@test"),

  {[a,b,c], []} = read("(a b c)"),
  {[a,b|c], []} = read("(a b . c)"),
  ?EXPECT(read("(a . b . c)"), missing_close_paren),
  ?EXPECT(read("(foo]"), invalid_close_paren),
  ?EXPECT(read("[foo)"), invalid_close_paren),

  {{a,b,c}, []} = read("#(a b c)"),
  ?EXPECT(read("#(a b . c)"), unexpected_dot),
  ?EXPECT(read("#(foo]"), invalid_close_paren),
  ?EXPECT(read("#[foo)"), {unexpected_hash_character, $[}),
  ?EXPECT(read("#"), unexpected_eof),

  {123, []} = read("123"),
  {3.14159, []} = read("3.14159"),
  {-135, []} = read("-135"),
  {246, []} = read("+246"),

  {true, []} = read("#t"),
  {false, []} = read("#f"),

  {['let', [[x,5]],['+',x,2]], "\n"} =
  read("
;; Commented code
(let ([x 5])
  (+ x 2))
"),

  ok.
