# JsonParserProlog

Dubini Alessandro 885957<br>
Foggetti Mattia 885958<br>
Martin Elia Leonardo 886366<br>

Introduzione

Lo sviluppo di applicazioni web su Internet, ma non solo, richiede di scambiare dati fra applicazioni
eterogenee, ad esempio tra un client web scritto in Javascript e un server, e viceversa. Uno standard
per lo scambio di dati molto diffuso è lo standard JavaScript Object Notation, o JSON. Lo scopo di
questo progetto è di realizzare due librerie, una in Prolog e l’altra in Common Lisp, che costruiscano
delle strutture dati che rappresentino degli oggetti JSON a partire dalla loro rappresentazione come
stringhe.

La descrizione dei metodi è contenuta nei commenti del codice.

La sintassi delle stringhe JSON

La sintassi JSON è definita nel sito https://www.json.org.
Dalla grammatica data, un oggetto JSON può essere scomposto ricorsivamente nelle seguenti parti:
1. Object
2. Array
3. Value
4. String
5. Number


Esempi Prolog

?- jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
 jsonaccess(O, ["nome"], R).
O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
R = ”Arthur”

?- jsonparse('{"nome": "Arthur", "cognome": "Dent"}', O),
 jsonaccess(O, "nome", R). % Notare le differenza.
O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
R = ”Arthur”

?- jsonparse('{"nome" : "Zaphod",
 "heads" : ["Head1", "Head2"]}', 
 Z),
 jsonaccess(Z, ["heads", 1], R).
Z = jsonobj([(”name”, ”Zaphod”), (”heads”, jsonarray([”Head1”, ”Head2”]))])
R = ”Head2”

?- jsonparse(’[]’, X).
X = jsonarray([]).

?- jsonparse(’{}’, X).
X = jsonobj([]).

?- jsonparse(’[}’, X).
false

?- jsonparse(’[1, 2, 3]’, A), jsonaccess(A, [3], E).
false
