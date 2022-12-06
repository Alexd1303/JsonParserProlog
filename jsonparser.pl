jsonread(FileName, JSON) :-
    string(FileName),
    open(FileName, read, In),
    read_string(In, _, Str),
    jsonparse(Str, JSON),
    !.
    %close(In).

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

jsonparse(['[' | Chs], V) :-
    value(['['| Chs], V, Rem).

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

key([],[],[]).%INV%
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
    parseint(Trimmed, R, Rest),
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
    whitespace(Chs, ChsTrimmed),
    parsearray(ChsTrimmed, R, Rem),
    whitespace(Rem, Trimmed).
value(['{' | Chs], jsonobj(R), Trimmed) :-
    !,
    object(['{' | Chs], R, Rem),
    whitespace(Rem, Trimmed).

stringparse(['"' | T], [], T).%INV%
stringparse([Ch | Chs], [Ch | R], Rest) :-
    stringparse(Chs, R, Rest).

parseint([',' | T], [], T).
%parseint(['}' | T], [], T).
%parseint([']' | T], [], [']' | T]).
parseint([Ch | T], [], [Ch | T]) :-
    \+(char_type(Ch, digit)), !.
parseint([Ch | Chs], [Ch | R], Rest) :-
    char_type(Ch, digit),
    parseint(Chs, R, Rest).

whitespace([], []).%INV%
whitespace([Ch | Chs], [Ch | Chs]) :-
    Ch \= ' ',
    Ch \= '\n',
    Ch \= '\r',
    Ch \= '\t'.
whitespace([_ | Chs], R) :-
    whitespace(Chs, R).

parsearray([']' | T], [], T).
parsearray(Chs, [V | R], Rest) :-
    whitespace(Chs, ChsTrimmed),
    value(ChsTrimmed, V, Rem),
    whitespace(Rem, RestTrimmed),
    parsearray(RestTrimmed, R, Rest).


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
    write(R6), nl,
    jsonread("./tests/test7.json", R7),
    write(R7), nl,
    jsonread("./tests/test8.json", R8),
    write(R8), nl.



jsonaccess(Jsonobj, Field, Result) :-
    string(Field),
    jsonaccess(Jsonobj, [Field], Result).

jsonaccess(R, [], R).
jsonaccess(jsonobj(L), [Field | Fields], Ric) :-
    string(Field),
    acceskey(L, Field, Result),
    jsonaccess(Result, Fields, Ric).
jsonaccess(jsonarray(L), [Field | Fields], Ric) :-
    integer(Field),
    length(L, Len),
    Field < Len,
    accesarray(L, Field, Result),
    jsonaccess(Result, Fields, Ric).

acceskey([], _, _):- !,fail.
acceskey([(Field, R) | _], Field, R).
acceskey([_ | R], Field, Result) :-
    acceskey(R, Field, Result).

accesarray([L | _], 0, L) :-
    L \= [].
accesarray([_ | Ls], Index, R) :-
    Cnt is Index - 1,
    accesarray(Ls, Cnt, R).

jsondump(JSON, FileName) :-
    jsonreverse(JSON, List),
    open(FileName, write, Out),
    atomic_list_concat(List, Str),
    write(Str),
    write(Out, Str),
    close(Out).


jsonreverse(jsonobj(L), ["{", Str, "}"]) :-
    reverseobj(L, R),
    atomic_list_concat(R, Str).

jsonreverse(jsonarray(L), ["[", Str, "]"]) :-
    reversearray(L, R),
    atomic_list_concat(R, Str).

reverseobj([], []).
reverseobj([(K, V)], ["\"", K, "\"", ':', VR]) :-
    reversevalue(V, VR).
reverseobj([(K, V) | L], ["\"", K, "\"", ':', VR, ',' | R]) :-
    reversevalue(V, VR),
    reverseobj(L, R).

reversearray([], []).
reversearray([V], [VR]) :-
    reversevalue(V, VR).
reversearray([V | L], [VR, ',' | R]) :-
    reversevalue(V, VR),
    reversearray(L, R).

reversevalue(V, VR) :-
    string(V),
    !,
    string_chars(V, L),
    append(['"' | L], ['"'], R),
    string_chars(VR, R).

reversevalue(V, VR) :-
    number(V),
    !,
    atom_string(V, VR).

reversevalue(V, VR) :-
    functor(V, jsonobj, 1),
    !,
    jsonreverse(V, L),
    atomic_list_concat(L, VR).

reversevalue(V, VR) :-
    functor(V, jsonarray, 1),
    !,
    jsonreverse(V, L),
    atomic_list_concat(L, VR).
