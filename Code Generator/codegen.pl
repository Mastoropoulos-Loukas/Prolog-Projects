iter_depth_first_search(State, States, Iter, Final) :-
	depth_first_search(State, [State], States, Iter, Final).

iter_depth_first_search(State, States, States, Iter, State) :- .

iter_depth_first_search(State1, SoFarStates, States, Iter, Final) :-
	Iter > 0,
	move(State1, State2),
	not member(State2, SoFarStates),
	append(SoFarStates, [State2], NewSoFarStates),
	NewIter is Iter - 1,
	depth_first_search(State2, NewSoFarStates, States, NewIter, Final).

move(State1, State2) :-
    State1 is State2 - 1.


search(Initial, Final, States) :-
	use_idfs(Initial, States, 0, Final).

use_idfs(Initial, States, Iter, Final) :-
	write("searching with max iterations ")
    write(Iter)
	iter_depth_first_search(Initial, States, Iter, Final).

use_idfs(Initial, States, Iter, Final) :-
	NewIter is Iter + 1,
	write("increasing iterations to ")
    write(NewIter)
	use_idfs(Initial, States, NewIter, Final).