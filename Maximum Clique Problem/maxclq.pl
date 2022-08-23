
% complementary(List, NewL, N):Find to complement of the edge set. List edge set, NewL the complement set, N the number of nodes in the Graph
complementary(List, NewL, N) :-
    findall(X1-X2, edge(X1, X2, N), L),
    removeall(List, L, NewL).

%edge(X1, X2, N): X1-X2 is a valid edge of a graph with N nodes.
edge(X1, X2, N) :-
    range(1, N, L1),
    member(X1, L1),
    NewX1 is X1 + 1,
    range(NewX1, N, L2),
    member(X2, L2).

% range(A, B, List): List = [A, A+1, ..., B]
range(A, A, [A]).
range(A, B, [A|Rest]) :-
    A < B,
    NewA is A + 1,
    range(NewA, B, Rest).

%removeall(List, L, NewL): NewL is the result of removing all elements of List from L (if they exist)
removeall([], L, L).
removeall([X|List], L, NewL) :-
    remove_if_exists(X, L, NewL1),
    removeall(List, NewL1, NewL).

%remove_if_exists(X, List, NewL): NewL is the result of removing X from List (if it exists)
remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
   !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).

% helper for debugging
run(Seed, N, D, Clique, Size) :-
    seed(Seed),
    maxclq(N, D, Clique, Size).


:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(branch_and_bound).

maxclq(N, D, Clique, Size) :- 
    create_graph(N, D, Graph), !,
    complementary(Graph, Comp, N),
    length(Nodes, N),
    Nodes #:: 0..1,
    costrain(Comp, Nodes),
    Cost #= sum(Nodes) * -1,
    bb_min( search(Nodes, 0, first_fail, indomain, complete, []), Cost, bb_options{}),
    getClique(Nodes, Clique, 1),
    length(Clique, Size).

% getClique(List, Clique, 1): Clique is a list of the indexes of items in List that have value 1. Used to translate values of binary variables to list of corresponding nodes.
getClique([], [], _).
getClique([L|List], Clique, Node) :-
    (
        L = 1 ->
        Clique = [Node | Rest]
    ;
        Clique = Rest
    ),
    NewNode is Node + 1,
    getClique(List, Rest, NewNode).

costrain([], _).
costrain([X1-X2 | Comp], Nodes) :-
    elementAtPos(Nodes, X1, N1),
    elementAtPos(Nodes, X2, N2),
    N1 + N2 #< 2,
    costrain(Comp, Nodes).

% elementAtPos(List, Pos, E): E is the item at position Pos of List.
elementAtPos([X|_], 1, X) :- !.
elementAtPos([_|List], Pos, E) :-
    Pos > 1,
    NewPos is Pos - 1,
    elementAtPos(List, NewPos, E).