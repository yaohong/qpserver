-module(protobuffs_parser).
-author('erlangonrails@gmail.com').
-export([parse_file/1]).

%% $simple.proto
%% package simple;
%%
%% message Person {
%%   required string name = 1;
%%   required string address = 2;
%%   required string phone_number = 3;
%%   required int32 age = 4;
%%   optional Location location = 5;
%% }
%%
%% message Location {
%%   required string region = 1;
%%   required string country = 2;
%% }
%%
%% parse_file("simple.proto")
%% 得到下面内容:
%% [{package,1},
%%  {bareword,1,"simple"},
%%  {';',1},
%%  {message,"Person",
%%           [{1,required,"string","name",number,none},
%%            {2,required,"string","address",number,none},
%%            {3,required,"string","phone_number",number,none},
%%            {4,required,"int32","age",number,none},
%%            {5,optional,"Location","location",number,none}]},
%%  {message,"Location",
%%           [{1,required,"string","region",number,none},
%%            {2,required,"string","country",number,none}]}]

-spec parse_file(Filename :: string()) ->
    [tuple()].
parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Raw = scan(binary_to_list(Data)),
    parse(Raw).

parse(Data) -> 
    parse(Data, []).

parse([], Acc) -> lists:reverse(Acc);
parse([{'}', _Line} | Tail], Acc) -> {Acc, Tail};
parse([{enum, _Line}, {bareword, _Line, MessageName}, {'{', _Line} | Tail], Acc) ->
    {Res, Tail2} = parse(Tail, []),
    parse(Tail2, [{enum, MessageName, lists:reverse(Res)} | Acc]);
parse([{message, _Line}, {bareword, _Line, MessageName}, {'{', _Line} | Tail], Acc) ->
    {Res, Tail2} = parse(Tail, []),
    parse(Tail2, [{message, MessageName, lists:reverse(Res)} | Acc]);
parse([{bareword, _Line, FieldName}, {'=', _Line}, {number, _Line, Value}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{enum, Value, FieldName} | Acc]);
parse([{Type, _Line}, {bareword, _Line, Field}, {bareword, _Line, FieldName}, {'=', _Line}, 
       {FieldType, _Line, Position}, {'[', _Line}, {bareword, _Line,"default"}, {'=', _Line}, 
       {_DefaultType, _Line, Default}, {']', _Line}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{Position, Type, Field, FieldName, FieldType, Default} | Acc]);
parse([{Type, _Line}, {bareword, _Line, Field}, {bareword, _Line, FieldName}, {'=', _Line}, 
       {FieldType, _Line, Position}, {';', _Line} | Tail], Acc) ->
    parse(Tail, [{Position, Type, Field, FieldName, FieldType, none} | Acc]);
parse([{'$end', _} | Tail], Acc) ->
    parse(Tail, Acc);
parse([Head | Tail], Acc) ->
    parse(Tail, [Head | Acc]).

scan(String) ->
    scan(String, [], 1).

scan([${|Rest], Accum, Line) ->
    scan(Rest, [{'{', Line}|Accum], Line);
scan([$}|Rest], Accum, Line) ->
    scan(Rest, [{'}', Line}|Accum], Line);
scan([$[|Rest], Accum, Line) ->
    scan(Rest, [{'[', Line}|Accum], Line);
scan([$]|Rest], Accum, Line) ->
    scan(Rest, [{']', Line}|Accum], Line);
scan([$(|Rest], Accum, Line) ->
    scan(Rest, [{'(', Line}|Accum], Line);
scan([$)|Rest], Accum, Line) ->
    scan(Rest, [{')', Line}|Accum], Line);
scan([$=|Rest], Accum, Line) ->
    scan(Rest, [{'=', Line}|Accum], Line);
scan([$;|Rest], Accum, Line) ->
    scan(Rest, [{';', Line}|Accum], Line);
scan([$,|Rest], Accum, Line) ->
    scan(Rest, [{',', Line}|Accum], Line);
scan([Digit|_] = String, Accum, Line)
  when Digit >= $0, Digit =< $9 ->
    {Number, Rest} = scan_number(String),
    scan(Rest, [{number, Line, Number}|Accum], Line);
scan([$-, Digit|_] = String, Accum, Line)
  when Digit >= $0, Digit =< $9 ->
    {Number, Rest} = scan_number(tl(String)),
    scan(Rest, [{number, Line, -Number}|Accum], Line);
scan([$\n|Rest], Accum, Line) ->
    scan(Rest, Accum, Line + 1);
scan([$\r,$\n|Rest], Accum, Line) ->
    scan(Rest, Accum, Line + 1);
scan([16#EF,16#BB, 16#BF|Rest], Accum, Line) ->
    scan(Rest, Accum, Line);
scan([WS|Rest], Accum, Line)
  when WS =:= 32; WS =:= $\t ->
    scan(Rest, Accum, Line);
scan([$/, $/|Rest], Accum, Line) ->
    scan(skip_to_newline(Rest), Accum, Line);
scan([$/, $*|Rest], Accum, Line) ->
    {Rest1, Line1} = skip_comment(Rest, Line),
    scan(Rest1, Accum, Line1);
scan([$"|_] = String, Accum, Line) ->
    {Strval, Rest, Line1} = scan_string(String, Line),
    scan(Rest, [{string, Line, Strval}|Accum], Line1);
scan([C|_] = String, Accum, Line)
  when C >= $A, C =< $Z;
       C >= $a, C =< $z;
       C =:= $_ ->
    {Identifier, Rest} = scan_identifier(String),
    Token = case get_keyword(Identifier) of
                Keyword when is_atom(Keyword) ->
                    {Keyword, Line};
                {bareword, Bareword} ->
                    {bareword, Line, Bareword}
            end,
    scan(Rest, [Token|Accum], Line);
scan([], Accum, Line) ->
    lists:reverse([{'$end', Line}|Accum]);
scan([C|_], _Accum, Line) ->
    erlang:error({invalid_character, [C], Line}).

scan_identifier(String) ->
    scan_identifier(String, "").

scan_identifier([C|Rest], Accum)
  when C >= $A, C =< $Z;
       C >= $a, C =< $z;
       C >= $0, C =< $9;
       C =:= $_;
       C =:= $. ->
    scan_identifier(Rest, [C|Accum]);
scan_identifier(Rest, Accum) ->
    {lists:reverse(Accum), Rest}.

scan_number(String) ->
    {A, Rest1} = scan_integer(String),
    case Rest1 of
        [$.|Fraction] ->
            {B, Rest2} = scan_identifier(Fraction),
            {A + list_to_float("0." ++ B), Rest2};
        [$e|Exp] ->
            {B, Rest2} = scan_integer(Exp),
            {list_to_float(integer_to_list(A) ++ ".0e" ++ integer_to_list(B)), Rest2};
        [$x|Rest] when A =:= 0 ->
            {Hex, Rest2} = scan_identifier(Rest),
            {erlang:list_to_integer(Hex, 16), Rest2};
        _ ->
            {A, Rest1}
    end.

scan_integer(String) ->
    scan_integer(String, 0).

scan_integer([D|Rest], Accum)
  when D >= $0, D =< $9 ->
    scan_integer(Rest, Accum * 10 + (D - $0));
scan_integer(Rest, Accum) ->
    {Accum, Rest}.

scan_string([$"|String], Line) ->
    scan_string(String, "", Line).

scan_string([$"|Rest], Accum, Line) ->
    {lists:reverse(Accum), Rest, Line};
scan_string([$\\, $a|Rest], Accum, Line) ->
    scan_string(Rest, [7|Accum], Line);
scan_string([$\\, $e|Rest], Accum, Line) ->
    scan_string(Rest, [$\e|Accum], Line);
scan_string([$\\, $f|Rest], Accum, Line) ->
    scan_string(Rest, [$\f|Accum], Line);
scan_string([$\\, $n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line);
scan_string([$\\, $r|Rest], Accum, Line) ->
    scan_string(Rest, [$\r|Accum], Line);
scan_string([$\\, $t|Rest], Accum, Line) ->
    scan_string(Rest, [$\t|Accum], Line);
scan_string([$\\, $v|Rest], Accum, Line) ->
    scan_string(Rest, [$\v|Accum], Line);
scan_string([$\\, D1, D2, D3|Rest], Accum, Line)
  when D1 >= $0, D1 =< $7, D2 >= $0, D2 =< $7, D3 >= $0, D3 =< $7 ->
    scan_string(Rest, [erlang:list_to_integer([D1, D2, D3], 8)|Accum], Line);
scan_string([$\\, $x, H1, H2|Rest], Accum, Line) ->
    scan_string(Rest, [erlang:list_to_integer([H1, H2], 16)|Accum], Line);
scan_string([$\\, Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line);
scan_string([$\n|Rest], Accum, Line) ->
    scan_string(Rest, [$\n|Accum], Line + 1);
scan_string([Char|Rest], Accum, Line) ->
    scan_string(Rest, [Char|Accum], Line).

skip_to_newline([$\n|Rest]) ->
    Rest;
skip_to_newline([]) ->
    [];
skip_to_newline([_|Rest]) ->
    skip_to_newline(Rest).

skip_comment([$*, $/|Rest], Line) ->
    {Rest, Line};
skip_comment([$\n|Rest], Line) ->
    skip_comment(Rest, Line + 1);
skip_comment([_|Rest], Line) ->
    skip_comment(Rest, Line).

get_keyword("import") ->
    import;
get_keyword("package") ->
    package;
get_keyword("option") ->
    option;
get_keyword("message") ->
    message;
get_keyword("group") ->
    group;
get_keyword("enum") ->
    enum;
get_keyword("extend") ->
    extend;
get_keyword("service") ->
    service;
get_keyword("rpc") ->
    rpc;
get_keyword("required") ->
    required;
get_keyword("optional") ->
    optional;
get_keyword("repeated") ->
    repeated;
get_keyword("returns") ->
    returns;
get_keyword("extensions") ->
    extensions;
get_keyword("max") ->
    max;
get_keyword("to") ->
    to;
get_keyword("true") ->
    true;
get_keyword("false") ->
    false;
get_keyword(Bareword) ->
    {bareword, Bareword}.


