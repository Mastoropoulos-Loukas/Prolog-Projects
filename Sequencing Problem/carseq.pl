% Loukas Mastoropoulos 1115 2017 00078

:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(ic_global).

getParams(N1, N2) :-
    classes(Classes),
    sum(Classes, N1),
    length(Classes, N2).

carseq(S) :-
    getParams(N1, N2),
    length(S, N1),
    S #:: 0..N2,
    classes(Classes),
    myOccurrences(S, Classes, 1),
    options(Options),
    mySequenceTotal(Options, S, Classes),
    search(S, 0, first_fail, indomain, complete, []).

% apply occurences predicate recursively
myOccurrences(_, [], _).
myOccurrences(S, [Class | Classes], I) :-
    occurrences(I, S, Class),
    NewI is I + 1,
    myOccurrences(S, Classes, NewI).

%transform list [A1, A2, ..., An] to [B1, B2, ..., Bn] where Bi = (Ai == 1) ? i : 0
transformOptions([], [], _).
transformOptions([X1 | L1], [X2 | L2], I) :-
    (
        X1 = 1 ->
        X2 = I
    ;
        X2 = 0
    ),
    NewI is I + 1,
    transformOptions(L1, L2, NewI).

dotAcc([], [], Acc, Acc).
dotAcc([X1 | L1], [X2 | L2], Acc, N) :-
    T is X1*X2,
    NewAcc is Acc + T,
    dotAcc(L1, L2, NewAcc, N).

%dot product of two vectors given as lists
dot(L1, L2, N) :-
    dotAcc(L1, L2, 0, N).

%apply sequence_total recursively
mySequenceTotal([], _, _).
mySequenceTotal([A/B/OptionList | Options], S, Classes) :-
    dot(Classes, OptionList, MinMax), %Min=Max=<Classes, OptionsList>
    transformOptions(OptionList, NewOptionsList, 1),
    sequence_total(MinMax, MinMax, 0, B, A, S, NewOptionsList),
    mySequenceTotal(Options, S, Classes).