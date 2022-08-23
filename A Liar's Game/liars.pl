:- set_flag(print_depth, 1000).
:- lib(ic).

liars(List, Liars) :-
    length(List, Size),
    length(Liars, Size),
    Liars #:: 0..1,
    costrain(Liars, List, Liars),
    search(Liars, 0, first_fail, indomain, complete, []).

costrain([], [], _).
costrain([L|Liars], [X | List], L1) :-
    L #= (X #> sum(L1)),
    costrain(Liars, List, L1).