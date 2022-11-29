jsonread(FileName, JSON) :-
    string(FileName),
    open(FileName, read, In),
    read_string(In, _, Str),
    jsonparse(Str, JSON).

jsonparse(JSONString, Object) :-
    atom(JSONString),
    !,
    atom_string(JSONString, Str),
    jsonparse(Str, Object).

jsonparse(JSONString, Object) :-
    string(JSONString),
    !,
    string_chars(JSONString, Chars),
    jsonparse(Chars, Object).

jsonparse(['{' | Chs], jsonobj(R)) :-
    object(['{' | Chs], R, _).

jsonparse(['[' | Chs], jsonarray(R)) :-
    array(Chs, R, _).

object([], [], []).
object(['}', ',' | T], [], T).
object(['}' | T], [], T).
object(['{' | Str], R, Rest) :-
    whitespace(Str, Trimmed),
    object(Trimmed, R, Rest).
object(Str, [P | R], Rest) :-
    pair(Str, P, Rem),
    object(Rem, R, Rest).

pair(Pair, (K, V), Rem) :-
    whitespace(Pair, KTrimmed),
    key(KTrimmed, K, Rest),
    whitespace(Rest, VTrimmed),
    value(VTrimmed, V, Rem).

key([':' | T], [], T).
key(['"' | Chs], Ks, Rem) :-
    stringparse(Chs, K, Rest),
    string_chars(Ks, K),
    whitespace(Rest, Trimmed),
    key(Trimmed, [], Rem).

value(['}' | T], [], ['}' | T]).
value([',' | T], [], T).
value([']' | T], [], [']' | T]).
value([Ch | Chs], I, Rest) :-
    Ch \= '"',
    char_type(Ch, digit),
    !,
    whitespace([Ch | Chs], Trimmed),
    integer(Trimmed, R, Rest),
    string_chars(S, R),
    atom_number(S, I).
value(['"'| Chs], S, Rem) :-
    !,
    stringparse(Chs, R, Rest),
    string_chars(S, R),
    whitespace(Rest, RestTrimmed),
    value(RestTrimmed, [], Rem).
value(['[' | Chs], jsonarray(R), Trimmed) :-
    !,
    array(Chs, R, Rem),
    whitespace(Rem, Trimmed).
value(['{' | Chs], jsonobj(R), Trimmed) :-
    !,
    object(['{' | Chs], R, Rem),
    whitespace(Rem, Trimmed).

stringparse(['"' | T], [], T).%INV%
stringparse([Ch | Chs], [Ch | R], Rest) :-
    stringparse(Chs, R, Rest).

integer([',' | T], [], T).%INV%
integer(['}' | T], [], T).
integer([']' | T], [], [']' | T]).
integer([Ch | Chs], [Ch | R], Rest) :-
    char_type(Ch, digit),
    integer(Chs, R, Rest).
    %whitespace(Rest, ChsTrimmed).

whitespace([], []).%INV%
whitespace([Ch | Chs], [Ch | Chs]) :-
    Ch \= ' ',
    Ch \= '\n',
    Ch \= '\r',
    Ch \= '\t'.
whitespace([_ | Chs], R) :-
    whitespace(Chs, R).

array([']' | T], [], T).
array(Chs, [V | R], Rest) :-
    whitespace(Chs, ChsTrimmed),
    value(ChsTrimmed, V, Rem),
    whitespace(Rem, RestTrimmed),
    array(RestTrimmed, R, Rest).


runtests() :-
    jsonread("./tests/test1.json", R1),
    write(R1), nl,
    jsonread("./tests/test2.json", R2),
    write(R2), nl,
    jsonread("./tests/test3.json", R3),
    write(R3), nl,
    jsonread("./tests/test4.json", R4),
    write(R4), nl,
    jsonread("./tests/test5.json", R5),
    write(R5), nl,
    jsonread("./tests/test6.json", R6),
    write(R6), nl.
