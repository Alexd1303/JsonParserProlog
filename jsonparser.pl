jsonparse(JSONString, Object) :-
    string(JSONString),
    !,
    string_chars(JSONString, Chars),
    jsonparse(Chars, Object).

jsonparse(['{' | Chs], jsonobj(R)) :-
    object(['{' | Chs], R).

%jsonparse(['[' | Chs], R) :-
%    array(Chs, R).

object([], []).
object(['}'], []).
object(['{' | Str], [P | R]) :-
    pair(Str, P, Rem),
    object(Rem, R).
object(Str, [P | R]) :-
    pair(Str, P, Rem),
    object(Rem, R).

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
value([Ch | Chs], I, Rest) :-
    Ch \= '"',
    !,
    integer([Ch | Chs], R, Rest),
    string_chars(S, R),
    atom_number(S, I).
value(['"'| Chs], S, Rem) :-
    !,
    stringparse(Chs, R, Rest),
    string_chars(S, R),
    whitespace(Rest, RestTrimmed),
    value(RestTrimmed, [], Rem).

stringparse(['"' | T], [], T).
stringparse([Ch | Chs], [Ch | R], Rest) :-
    stringparse(Chs, R, Rest).

integer([',' | T], [], T).
integer(['}' | T], [], T).
integer([Ch | Chs], [Ch | R], Rest) :-
    char_type(Ch, digit),
    whitespace(Chs, ChsTrimmed),
    integer(ChsTrimmed, R, Rest).

whitespace([Ch | Chs], [Ch | Chs]) :-
    Ch \= ' ',
    Ch \= '\n',
    Ch \= '\r',
    Ch \= '\t'.
whitespace([Ch | Chs], R) :-
    whitespace(Chs, R).


jsonarray().