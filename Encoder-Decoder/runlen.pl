expand((X, 1), [X]).
expand((X, N), [X|L]) :-
    N > 0,
    N1 is N - 1,
    expand((X, N1), L).

decode_rl([], []) :- !.
decode_rl([Head|Tail], DecodedList) :-
    !,
    decode_rl(Head, DecodedList1),
    decode_rl(Tail, DecodedList2),
    append(DecodedList1, DecodedList2, DecodedList).
decode_rl((X, N), DecodedList) :-
    !,
    expand((X, N), DecodedList).
decode_rl(X, [X]).

find_prefix([X], [X], []).
find_prefix([X, X|List], [X|Pref], Rest) :-
    find_prefix([X|List], Pref, Rest).
find_prefix([X, Y|List], [X], [Y|List]) :-
    X \= Y.

/*
collapseAcc([], (_, Acc), Acc).
collapseAcc([X|Tail], (X, N), Acc):-
    Acc1 is Acc + 1,
    collapseAcc(Tail, (X, N), Acc1).*/


collapse([X], X) :- !.
collapse([Head|Tail], (Head, N)):-
    !,
    N is length([Head|Tail]).
    /*List = [Head|Tail],
    collapseAcc(List, (X, N), 0).*/

encode_rl([], []).
encode_rl(List, [EncodedPref|EncodedList]) :-
    find_prefix(List, Pref, Rest),
    collapse(Pref, CollapsedPref),
    EncodedPref = CollapsedPref,
    encode_rl(Rest, EncodedList).


/*
?- encode_rl([p(3), p(X), q(X), q(Y), q(4)], L).
X = 3
Y = 3
L = [(p(3), 2), (q(3), 2), q(4)]


Εξήγηση:
Η encode_rl/2 παράγει τον στόχο find_prefix/2 στην με πρώτο όρισμα την λίστα [p(3), p(X), p(X), q(Y), q(4)].
Η πρώτη περίπτωση (λίστα με ένα στοιχείο) δεν ταιριάζει. Πάει στην δεύτερη.
Προσπαθεί να βρεί ανάθεση όπου θα ταιριάξει η δεύτερη περίπτωση, δηλαδή προσαθεί να ενοποιήσει το p(3) με το p(X), ώστε να είναι η λίστα της μορφής [Χ, Χ|List]. Αυτό γίνεται αν X = 3 (το Χ στο στοιχείο p(X)). To X (το Χ στο στοιχείο p(X)) παίρνει την τιμή 3. Άρα το κατηγόρημα αναθέτει ως πρόθεμα Pref την λίστα [p(3), p(3), p(3)] (=[p(3), p(3), p(3)] με Χ = 3).
To κατηγόρημα collapse (επόμενος στόχος), έχει ως πρώτο όρισμα την λίστα [p(3), p(3), p(3)] και αναθέτει στο δεύτερο όρισμα (CollapsedPref) την τιμή [(p(3), 3)]. 

H υπόλοιπη λίστα είναι [q(3), q(Y), q(4)] (Το Χ έχει πάρει την τιμή 3).
Καλείται το find_pref/3 με πρώτο όρισμα την παραπάνω λίστα.
Η πρώτη περίπτωση δεν ικανοποιείται. Στη δεύτερη προσπαθεί να ενοποιείσει το q(3) και το q(Y), το οποίο γίνεται όταν Υ=3, άρα το όρισμα Pref παίρνει την τιμή [q(3), q(3)] (και το Rest την τιμή [q(4)]). 

Από την collapse στις λίστες [q(3), q(3)] και [q(4)] παίρνουμε (q(3), 2) και q(4) αντίστοιχα.

Τελικά προκύπτει το αποτέλεσμα L = [(p(3), 2), (q(3), 2), q(4)]
*/ 