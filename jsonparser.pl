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
    object(['{' | Chs], R, RemTrimmed).

jsonparse(['[' | Chs], R) :-
    array(Chs, R, _).

object(['{', '}' | T], [], T).
object([], [], []).
object(['}' | T], [], T).
object(['{' | Str], [P | R], RemTrimmed) :-
    pair(Str, P, Rem),
    object(Rem, R, RemTrimmed).
object(Str, [P | R], RemTrimmed) :-
    pair(Str, P, Rem),
    object(Rem, R, RemTrimmed).

pair(Pair, (K, V), Rem) :-
    whitespace(Pair, KTrimmed),
    key(KTrimmed, K, Rest),
    whitespace(Rest, VTrimmed),
    value(VTrimmed, V, Rem).

key([':' | T], [], T).
key(['"' | Chs], Ks, Rem) :-
    stringparse(Chs, K, Rest),
    string_chars(Ks, K),
    whitespace(Rest, Trimmmed),
    key(Trimmmed, [], Rem).

value(['}' | T], [], T).
value([',' | T], [], T).
value([']' | T], [], [']' | T]).
value([Ch | Chs], I, Rest) :-
    Ch \= '"',
    integer([Ch | Chs], R, Rest),
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
    object(['{' | Chs], R, Rem),
    whitespace(Rem, Trimmed).

stringparse(['"' | T], [], T).
stringparse([Ch | Chs], [Ch | R], Rest) :-
    stringparse(Chs, R, Rest).

integer([',' | T], [], T).
integer(['}' | T], [], T).
integer([']' | T], [], [']' | T]).
integer([Ch | Chs], [Ch | R], Rest) :-
    char_type(Ch, digit),
    whitespace(Chs, ChsTrimmed),
    integer(ChsTrimmed, R, Rest).

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
    array(Rem, R, Rest).
