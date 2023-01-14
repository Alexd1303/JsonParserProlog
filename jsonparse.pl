%Dubini Alessandro 885957
%Foggetti Mattia 885958
%Elia Leonardo Martin 886366

%Regola per la lettura da file.
%Vera se dalla stringa contenuta
%nel file con percorso FileName
%è possibile costruire l'oggetto JSON
jsonread(FileName, JSON) :-
    open(FileName, read, In),
    read_string(In, _, Str),
    jsonparse(Str, JSON),
    !.

%Vera se dalla stringa JSONString è
%possibile costruire l'oggetto Object
jsonparse(JSONString, Object) :-
    string(JSONString),
    !,
    string_chars(JSONString, CharList),
    json(Object, CharList, []).

%Vera se dall'atomo JSONString è
%possibile costruire l'oggetto Object
jsonparse(JSONString, Object) :-
    atom(JSONString),
    !,
    atom_string(JSONString, String),
    string_chars(String, CharList),
    json(Object, CharList, []).

%Definizione della struttura di un JSON.
%Può essere un oggetto, contenente coppie chiave-valore,
%o un array, contente valori di qualsiasi tipo
json(JSON) --> object(JSON), !; array(JSON), !. 

%Definizione della struttura di un oggetto JSON.
%Può essere vuoto o contenere delle coppie chiave-valore
object(jsonobj([])) --> skips, ['{'], skips, ['}'], skips.
object(jsonobj(Ms)) --> skips, ['{'], skips, members(Ms), skips, ['}'], skips.

%Definizione della struttura di un array.
%Può essere vuoto o contenere dei valori di qualsiasi tipo
array(jsonarray([])) --> skips, ['['], skips, [']'], skips.
array(jsonarray(Vs)) --> skips, ['['], skips, values(Vs), skips, [']'], skips.

%Definizione della struttura dei valori di un array non vuoto.
%Può essere uno o più valori separati da virgola
values([V]) --> value(V).
values([V | Vs]) --> skips, value(V), skips, [','], skips, values(Vs).

%Definizione della struttura delle coppie di un oggetto non vuoto.
%Può essere una o più coppie chiave-valore
members([M]) --> member(M).
members([M | Ms]) --> member(M), skips, [','], skips, members(Ms).

%Definizione della struttura di una coppia chiave-valore.
%Può essere una o più coppie separate da virgola
member((K, V)) --> skips, key(K), skips, [':'], skips, value(V), skips.

%Definizione della struttura di una chiave.
%Deve essere necessariamente una stringa
key(Vs) --> stringToken(V), {string_chars(Vs, V)}.

%Definizione dei possibili valori contenuti in un JSON.
value(Vs) --> stringToken(V), {string_chars(Vs, V)}.
value(I) --> numberToken(V), {string_chars(Vs, V), atom_number(Vs, I)}.
value(N) --> expNumberToken(V), {string_chars(Vs, V), atom_number(Vs, N)}.
value(O) --> object(O).
value(A) --> array(A).
value(Bool) --> [t, r, u, e], {Bool = true}; [f, a, l, s, e], {Bool = false}.
value(Null) --> [n, u, l, l], {Null = null}.

%Definizione della struttura di una stringa.
stringToken(S) --> skips, ['"'], charsToken(S), ['"'], skips.
charsToken([Ch]) --> [Ch], {Ch \= '"'}.
charsToken([Ch | Chs]) --> [Ch], {Ch \= '"'}, charsToken(Chs).
charsToken([Ch | Chs]) --> ['\\'], [Ch], charsToken(Chs).

%Definizione della struttura di un numero.
%Sono accettati i numeri interi e decimali sia positivi che negativi
numberToken(N) --> skips, digits(N), skips.
numberToken(N) --> skips, digits(I), ['.'], digits(D),
		   skips, {append(I, ['.' | D], N)}.
numberToken(N) --> skips, digits(N).
numberToken(['-' | N]) --> skips, ['-'], !, numberToken(N), skips.
digits([D]) --> digit(D).
digits([D | Ds]) --> digit(D), !, digits(Ds).
digit(D) --> [D], {char_type(D, digit)}.
expNumberToken(N) --> skips, numberToken(B), ['e'],
		      digits(E), skips, {append(B, ['e' | E], N)}.
expNumberToken(N) --> skips, numberToken(B), ['e'],
		      ['-'], digits(E), skips, {append(B, ['e', '-' | E], N)}.

%Definizione dei caratteri whitespace
skips --> [].
skips --> skip.
skips --> skip, !, skips.
skip --> ['\n']; [' ']; ['\r']; ['\t'].

%Vero se è possibile ottenere Result da Jsonobj
%accedendo ai campi indicati in Fields
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

%Vero se la coppia chiave-valore ha come chiave quella desiderata
accesskey([], _, _) :- fail.
accesskey([(Key, Value) | _], Key, Value).
accesskey([(Key, _) | Members], Field, Result) :-
    Key \= Field,
    accesskey(Members, Field, Result).

%Vero se Value si trova nella posizione dell'array indicata da Index
accessarray([Value | _], 0, Value).
accessarray([_ | Values], Index, Result) :-
    Count is Index - 1,
    accessarray(Values, Count, Result).

%Vero se l'oggetto JSON viene scritto correttamente
%sul file il cui percorso è FileName
jsondump(JSON, FileName) :-
    jsonreverse(JSON, ReversedObject),
    !,
    open(FileName, write, Out),
    write(Out, ReversedObject),
    close(Out).

%Vero se Reversed contiene la stringa che rappresenta il JSON
jsonreverse(jsonobj(Members), Reversed) :-
    reverseobj(Members, Result),
    !,
    atomic_list_concat(Result, String),
    format(string(Reversed), '{~s}', String).
jsonreverse(jsonarray(Values), Reversed) :-
    reversearray(Values, Result),
    !,
    atomic_list_concat(Result, String),
    format(string(Reversed), '[~s]', String).

%Vera se il secondo argomento è la lista 
%degli atomi che rappresentano l'oggetto
reverseobj([], []).
reverseobj([(K, V)], ['"', K, '"', ':', VReversed]) :-
    reversevalue(V, VReversed).
reverseobj([(K, V) | Members], ['"', K, '"', ':', VReversed, ',' | R]) :-
    reversevalue(V, VReversed),
    reverseobj(Members, R).

%Vera se il secondo argomento è la lista degli atomi che rappresentano l'array
reversearray([], []).
reversearray([Value], [VReversed]) :-
    reversevalue(Value, VReversed).
reversearray([Value | Values], [VReversed, ',' | R]) :-
    reversevalue(Value, VReversed),
    reversearray(Values, R).

%Vero se il secondo argomento è la stringa che rappresenta Value
reversevalue(Value, VReversed) :-
    string(Value),
    !,
    fixstring(Value, Fixed),
    format(string(VReversed), '"~s"', Fixed).
reversevalue(Value, VReversed) :-
    number(Value),
    !,
    atom_string(Value, VReversed).
reversevalue(Value, VReversed) :-
    functor(Value, jsonobj, 1),
    !,
    jsonreverse(Value, VReversed).
reversevalue(Value, VReversed) :-
    functor(Value, jsonarray, 1),
    !,
    jsonreverse(Value, VReversed).

%Vero se Fixed è la stringa rappresentata da
%Str con i caratteri di escape corretti
fixstring(Str, Fixed) :-
    string(Str),
    !,
    string_chars(Str, Chs),
    fixstring(Chs, FChs),
    string_chars(Fixed, FChs).
fixstring([], []).
fixstring(['"' | Chs], ['\\', '"' | Fixed]) :-
    !,
    fixstring(Chs, Fixed).
fixstring([Ch | Chs], [Ch | Fixed]) :-
    Ch \= '"',
    !,
    fixstring(Chs, Fixed).
fixstring(['\\' | Chs], ['\\', '\\' | Fixed]) :-
    !,
    fixstring(Chs, Fixed).
fixstring([Ch | Chs], [Ch | Fixed]) :-
    Ch \= '\\',
    !,
    fixstring(Chs, Fixed).
