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
    key(Pair, K, Rest),
    value(Rest, V, Rem).

key([':' | T], [], T).
key(['"' | Chs], Ks, Rem) :-
    valstr(Chs, K, Rest),
    string_chars(Ks, K),
    key(Rest, [], Rem).

value(['}' | T], [], T).
value([',' | T], [], T).
%value([Ch | Chs], R, Rest) :-
%    integer(Chs, R, Rest).
value(['"'| Chs], S, Rem) :-
    valstr(Chs, R, Rest),
    string_chars(S, R),
    value(Rest, [], Rem).

valstr(['"' | T], [], T).
valstr([Ch | Chs], [Ch | R], Rest) :-
    valstr(Chs, R, Rest).

jsonarray().