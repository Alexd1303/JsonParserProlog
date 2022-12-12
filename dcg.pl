jsonread(FileName, JSON) :-
    string(FileName),
    open(FileName, read, In),
    read_string(In, _, Str),
    jsonparse(Str, JSON),
    !.

jsonparse(JSONString, Object) :-
    string(JSONString),
    !,
    string_chars(JSONString, CharList),
    json(Object, CharList, []).

jsonparse(JSONString, Object) :-
    atom(JSONString),
    !,
    atom_string(JSONString, String),
    string_chars(String, CharList),
    json(Object, CharList, []).

json(JSON) --> object(JSON); array(JSON). 

object(jsonobj([])) --> skips, ['{'], skips, ['}'], skips.
object(jsonobj(Members)) --> skips, ['{'], skips, members(Members), skips, ['}'], skips.

array(jsonarray([])) --> skips, ['['], skips, [']'], skips.
array(jsonarray(Values)) --> skips, ['['], skips, values(Values), skips, [']'], skips.

values([Value]) --> value(Value).
values([Value | Values]) --> skips, value(Value), skips, [','], skips, values(Values).

members([Member]) --> member(Member).
members([Member | Members]) --> member(Member), skips, [','], skips, members(Members).

member((K, V)) --> skips, key(K), skips, [':'], skips, value(V), skips.

key(Vs) --> stringToken(V), {string_chars(Vs, V)}.

value(Vs) --> stringToken(V), {string_chars(Vs, V)}.
value(I) --> numberToken(V), {string_chars(Vs, V), atom_number(Vs, I)}.
value(O) --> object(O).
value(A) --> array(A).
value(Bool) --> [t, r, u, e], {Bool = true}; [f, a, l, s, e], {Bool = false}.
value(Null) --> [n, u, l, l], {Null = null}.

stringToken(S) --> skips, ['"'], charsToken(S), ['"'], skips.
charsToken([Ch]) --> [Ch], {Ch \= '"'}.
charsToken([Ch| Chs]) --> [Ch], {Ch \= '"'}, charsToken(Chs).

numberToken(N) --> skips, digits(N), skips.
numberToken(N) --> skips, digits(N).
digits([D]) --> digit(D).
digits([D | Ds]) --> digit(D), !, digits(Ds).
digit(D) --> [D], {char_type(D, digit)}.

skips --> [].
skips --> skip.
skips --> skip, !, skips.
skip --> ['\n']; [' ']; ['\r']; ['\t'].


jsonaccess(Jsonobj, Field, Result) :-
    string(Field),
    jsonaccess(Jsonobj, [Field], Result).

jsonaccess(Value, [], Value).
jsonaccess(jsonobj(Members), [Field | Fields], Result) :-
    string(Field),
    accesskey(Members, Field, Value),
    jsonaccess(Value, Fields, Result).
jsonaccess(jsonarray(Values), [Field | Fields], Result) :-
    integer(Field),
    length(Values, Len),
    Field > -1,
    Field < Len,
    accessarray(Values, Field, Value),
    jsonaccess(Value, Fields, Result).

accesskey([], _, _) :- fail.
accesskey([(Key, Value) | _], Key, Value).
accesskey([(Key, _) | Members], Field, Result) :-
    Key \= Field,
    accesskey(Members, Field, Result).

accessarray([Value | _], 0, Value).
accessarray([_ | Values], Index, Result) :-
    Count is Index - 1,
    accessarray(Values, Count, Result).


jsondump(JSON, FileName) :-
    jsonreverse(JSON, List),
    open(FileName, write, Out),
    atomic_list_concat(List, Str),
    write(Out, Str),
    close(Out).

jsonreverse(jsonobj(Members), ["{", String, "}"]) :-
    reverseobj(Members, Result),
    atomic_list_concat(Result, String).

jsonreverse(jsonarray(Values), ["[", String, "]"]) :-
    reversearray(Values, Result),
    atomic_list_concat(Result, String).

reverseobj([], []).
reverseobj([(K, V)], ["\"", K, "\"", ':', VReversed]) :-
    reversevalue(V, VReversed).
reverseobj([(K, V) | Members], ["\"", K, "\"", ':', VReversed, ',' | R]) :-
    reversevalue(V, VReversed),
    reverseobj(Members, R).

reversearray([], []).
reversearray([Value], [VReversed]) :-
    reversevalue(Value, VReversed).
reversearray([Value | Values], [VReversed, ',' | R]) :-
    reversevalue(Value, VReversed),
    reversearray(Values, R).

reversevalue(Value, VReversed) :-
    string(Value),
    !,
    string_chars(Value, CharList),
    append(['"' | CharList], ['"'], R),
    string_chars(VReversed, R).

reversevalue(Value, VReversed) :-
    number(Value),
    !,
    atom_string(Value, VReversed).

reversevalue(Value, VReversed) :-
    functor(Value, jsonobj, 1),
    !,
    jsonreverse(Value, R),
    atomic_list_concat(R, VReversed).

reversevalue(Value, VReversed) :-
    functor(Value, jsonarray, 1),
    !,
    jsonreverse(Value, R),
    atomic_list_concat(R, VReversed).

