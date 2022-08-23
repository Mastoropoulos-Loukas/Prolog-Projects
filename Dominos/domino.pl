/*
Τα ντόμινο τα αναπαριστούμε με συναρτησιακά σύμβολα της μορφής Domino = (ValueA, ValueB, Direction),
όπου ValueA η τιμή μίας άκρης, ValueB η τιμή της άλλης άκρης και Direction ανήκει στο {left, right, up, down}. 

Την θέση ενός ντόμινο την αναπαριστούμε με συναρτησιακά σύμβολα της μορφής Position = (X, Y) όπου Χ και Υ οι συντεταγμένες στο frame της άκρης με τιμή ValueA του ντόμινο. Δηλαδή αν το ντόμινο (3, 2, right) το βάλαμε στην θέση (2,3), τότε στο δωσμένο frame υπάρχει στην θέση (2,3) το 3 και στην (3,3) το 2 (η αρίθμηση για τα X και Υ στο frame ξεκκινάει από 1) 
*/

/*

Σχόλια για την άσκηση:

Τα αποτελέσματα βγαίνουν σωστά, όμως η εκτύπωση με την μορφή που παρουσιάζεται στην εκφώνηση έχει προβλήματα σε μεγαλύτερα στυγμιότυπα.

Για παράδειγμα το put_dominos (που εκτυπώνει στην κονσόλα όπως φαίνεται στην εκφώνηση) για το 8 X 7 frame, δεν τελειώνει σε εύλογο χρόνο, όμως το 
put_dominos_ugly_print, τελειώνει σε 0.02s και παρουσιάζει σωστά αποτελέσματα
*/


% ------------------------------------------------------------------------
% Small Testing Example (2 Χ 2)

% frame([
%     [1,2],
%     [3,4]
% ]).

% dominos([(2,4), (1,3)]).

% FC :      (0.00s cpu)
% FC + MRV: (0.00s cpu)
%--------------------------------------------------------------------------
%   (4 X 3)

% dominos([(0,0),	(0,1),	(0,2),
% 				(1,1),	(1,2),
% 						(2,2)]).
		 

%  frame([[1,1,2,1],
% 		[1,2,2,0],
% 		[0,0,2,0]]).

% FC :      (0.00s cpu)
% FC + MRV: (0.00s cpu)

% -------------------------------------------------------------------------
%   Example 1 - Εκφώνησης (8 Χ 7)

% dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
%  (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
%  (2,2),(2,3),(2,4),(2,5),(2,6),
%  (3,3),(3,4),(3,5),(3,6),
%  (4,4),(4,5),(4,6),
%  (5,5),(5,6),
%  (6,6)]).

% frame([[3,1,2,6,6,1,2,2],
%  [3,4,1,5,3,0,3,6],
%  [5,6,6,1,2,4,5,0],
%  [5,6,4,1,3,3,0,0],
%  [6,1,0,6,3,2,4,0],
%  [4,1,5,2,4,3,5,5],
%  [4,1,0,2,4,5,2,0]]).

% FC + MRV : (0.02s cpu).
%--------------------------------------------------------------------------
%   Example 2 - Lists (11 Χ 12)

dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),
               (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),
                     (2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),
                           (3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),
                                 (4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),
                                       (5,5),(5,6),(5,7),(5,8),(5,9),(5,a),
                                             (6,6),(6,7),(6,8),(6,9),(6,a),
                                                   (7,7),(7,8),(7,9),(7,a),
                                                         (8,8),(8,9),(8,a),
                                                               (9,9),(9,a),
                                                                     (a,a)]).

frame([[6,5,0,5,5,3,3,1,1,4,6],
       [1,2,2,a,a,5,7,0,1,0,7],
       [5,8,6,0,8,0,9,7,7,4,2],
       [4,0,9,0,7,7,9,9,8,8,0],
       [1,a,3,8,8,5,a,8,0,0,3],
       [9,2,3,5,7,6,9,1,6,3,9],
       [2,2,2,5,8,6,0,4,6,a,a],
       [9,4,2,1,7,9,5,4,a,4,a],
       [9,a,4,9,5,5,6,6,0,a,2],
       [1,a,1,2,1,1,8,2,2,7,8],
       [7,7,3,3,4,3,6,6,4,3,1],
       [5,9,6,3,3,a,7,4,4,8,8]]).

% FC + MRV : (>12m cpu).
%--------------------------------------------------------------------------
directions([right, down, left, up]).

getDominos(Dominos) :-
    dominos(Dominos).

numOfDominos(N) :-
    getDominos(Dominos),
    length(Dominos, N).

domino(Pos, Domino) :-
    getDominos(Dominos),
    listItemAtPos(Dominos, Pos, Domino).

% -------------------------------------------------------------------------
%   Helpers

getDirections(Directions) :-
    directions(Directions).
% getDirections/1
% getDirections(Directions) είναι αληθές αν έχει οριστεί η λίστα directions σαν κατηγόρημα, και επιστρέφει τη λίστα στο Directions. π.χ. (για directions([right, down, left, up]). ):
% ?- getDirections(Directons).
% Directions = [right, down, left, up]

getFrameTable(Table) :-
    frame(Table).

% getFrameTable/1
% getFrameTable(Table) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει το frame σαν πίνακα στο Table. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getFrameTable(Table).
% Table = [[1,2],[3,4],[5,6]]

getFrameWidth(Width) :-
    getFrameTable(Table),
    listItemAtPos(Table, 1, Row),
    length(Row, Width).

% getFrameWidth/1
% getFrameWidth(Width) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει τo πλάτος του πίνακα στο Width. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getFrameWidth(Width).
% Width = 2

getFrameHeight(Height) :-
    getFrameTable(Table),
    length(Table, Height).

% getFrameHeight/1
% getFrameHeight(Height) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει το ύψος του πίνακα στο Height. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getFrameHeight(Height).
% Height = 3

getFrameWidthHeight(Width, Height) :-
    getFrameWidth(Width),
    getFrameHeight(Height).

% getFrameWidthHeight/2
% getFrameWidthHeight(Width, Height) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει τo πλάτος του πίνακα στο Width και το ύψος του πίνακα στο Height. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getFrameWidthHeight(Width, Height).
% Width = 2
% Height = 3

getXList(XList) :-
    getFrameWidth(Width),
    range(1, Width, XList).

% getXList/1
% getXList(XList) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει μία λίστα ΧList με όλες τις δυνατές τιμές της Χ-συντεταγμένης. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getXList(XList).
% XList = [1, 2] 

getYList(YList) :-
    getFrameHeight(Height),
    range(1, Height, YList).

% getYList/1
% getYList(YList) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει μία λίστα YList με όλες τις δυνατές τιμές της Y-συντεταγμένης. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getYList(YList).
% YList = [1, 2, 3] 

getXYLists(XList, YList) :-
    getXList(XList),
    getYList(YList).

% getXYLists/2
% getXYLists(Xlist, YList) είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και επιστρέφει μία λίστα ΧList με όλες τις δυνατές τιμές της Χ-συντεταγμένης και μία λίστα YList με όλες τις δυνατές τιμές της Y-συντεταγμένης. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- getΧYLists(XList, YList).
% XList = [1, 2]
% YList = [1, 2, 3] 

printRow([]) :-
    write('\n').
printRow([Row|Rest]) :-
    write(' '),
    write(Row),
    printRow(Rest).

% printRow/1
% printRow(Row) είναι αληθές αν Row είναι λίστα, και εκτυπώνει στο console την λίστα σαν γραμμή αντικειμένων. π.χ.:
% ?- printRow([1,2,3]).
% 1 2 3

printTable([]).
printTable([Row|Rest]) :-
    printRow(Row),
    printTable(Rest).

% printTable/1
% printTable(Table) είναι αληθές αν Table είναι πίνακας ως λίστα από λίστες, και εκτυπώνει στο console τον πίνακα. π.χ.:
% ?- printTable([[1,2],[3,4],[5,6]]).
% 1 2
% 3 4
% 5 6

printFrame :-
    getFrameTable(T),
    printTable(T).

% printFrame/0
% printFrame είναι αληθές αν έχει οριστεί το frame σαν κατηγόρημα, και εκτυπώνει στο console τον πίνακα. π.χ. (για frame([[1,2],[3,4],[5,6]]). ):
% ?- printFrame.
% 1 2
% 3 4
% 5 6

listItemAtPos([X|_], 1, X).
listItemAtPos([_|Rest], Pos, Item) :-
    Pos > 1,
    NewPos is Pos-1,
    listItemAtPos(Rest, NewPos, Item).

% listItemAtPos/3
% listItemAtPos(List, Pos, Item) είναι αληθές αν στην λίστα List στην θέση Pos βρισκεται το Item (η αρίθμηση ξεκκινάει από 1). π.χ.:
% ?- listItemAtPos([a,b,c], 2, Item). 
% Item = b

tableItemAtPos(Table, (X, Y), Item) :-
    listItemAtPos(Table, Y, Row),
    listItemAtPos(Row, X, Item).

% tableItemAtPos/3
% tableItemAtPos(Table, Pos, Item) είναι αληθές αν στον πίνακαα Table στην θέση Pos = (X, Y) βρισκεται το Item (η αρίθμηση ξεκκινάει από 1). π.χ.:
% ?- tableItemAtPos([[a,b],[c,d],[e,f]], (2,2), Item). 
% Item = d

posOfValueB((X, Y), Direction, PosB) :-
    (
        Direction = right,
        NewX is X + 1,
        PosB = (NewX, Y)
    )
    ;
    (
        Direction = left,
        NewX is X - 1,
        PosB = (NewX, Y)
    )
    ;
    (
        Direction = up,
        NewY is Y - 1,
        PosB = (X, NewY)
    )
    ;
    (
        Direction = down,
        NewY is Y + 1,
        PosB = (X, NewY)
    ).

% posOfValueB/3
% posOfValueB(PosA, Direction, PosB) είναι αληθές αν για ντόμινο (ValueA, ValueB) με την άκρη με τιμή ValueA στην θέση PosA και κατεύθυνση Direction (που ανήκει στο {left, right, up, down}) η άκρη με τιμή ValueB είναι στην θέση PosB. PosA και PosB της μορφής (X, Y). π.χ.:
% ?- posOfValueB((2,2), right, PosB).
% PosB = (3,2)

properPosition((ValueA, ValueB), FrameTable, Position, Direction) :-
    tableItemAtPos(FrameTable, Position, ValueA),
    posOfValueB(Position, Direction, PosB),
    tableItemAtPos(FrameTable, PosB, ValueB).

% properPosition/4
% properPosition(Domino, FrameTable, Position, Direction) είναι αληθές αν το Domino, στην θέση Position με κατεύθυνση Direction είναι σε έγκυρη θέση με βάση τον πίνακα FrameTable. Η θέση Position πρέπει να δίνεται, δεν γίνεται δηλαδή να είναι μεταβλητή. π.χ.:
% ?- properPosition(Domino, [[1,2],[3,4]], (2,1), down).
% Domino = (2,4)

properPos(Domino, Position, Direction) :-
    getFrameTable(Table),
    properPosition(Domino, Table, Position, Direction).

% properPos/3
% properPos(Domino, Position, Direction) είναι αληθές αν το Domino, στην θέση Position με κατεύθυνση Direction είναι σε έγκυρη θέση με βάση τον πίνακα που ορίζεται σαν κατηγόρημα. Η θέση Position πρέπει να δίνεται, δεν γίνεται δηλαδή να είναι μεταβλητή. π.χ.:
% ?- properPos(Domino, (2,1), down).
% Domino = (2,4)

range(A, A, [A]).
range(A, B, [A|Rest]) :-
    A < B,
    NewA is A + 1,
    range(NewA, B, Rest).

% range/3
% range(A, B, List) είναι αληθές αν η λίστα List είναι της μορφής [Α, Α+1, ..., Β]. π.χ.
% ?- range(1,3,List).
% List = [1,2,3]

findPosition(Domino, FrameTable, Position, Direction) :-
    getDirections(Directions),
    member(Direction, Directions),
    getXYLists(XList, YList),
    member(X, XList),
    member(Y, YList),
    properPosition(Domino, FrameTable, (X,Y), Direction),
    Position = (X,Y).

% findPosition/4
% findPosition(Domino, Table, Position, Direction) είναι αληθές αν το Domino, στην θέση Position με κατεύθυνση Direction είναι σε έγκυρη θέση με βάση τον πίνακα που ορίζεται σαν κατηγόρημα. Η θέση Position μπορεί να είναι μεταβλητή. π.χ.:
% ?- findPosition((2,4), [[1,2],[3,4]], Pos, down).
% Pos = (2,1)

domainValue((X, Y, Direction)) :-
    getXYLists(XList, YList),
    getDirections(Directions),
    member(X, XList),
    member(Y, YList),
    member(Direction, Directions).

% domainValue/3
% domainValue(X, Y, Direction) είναι αληθές αν (Χ, Υ) είναι έγκυρη θέση στο frame και αν η κατεύθυνση Direction είναι έγκυρη. π.χ. (για frame([[1,2],[3,4],[5,6]]). και directions([right, down, left, up]). ):
% ?- domainValue((1,2,down)).
% Yes

initialDomain(Domain) :-
    findall((X,Y,Direction), domainValue((X,Y,Direction)), Domain).
% initialDomain/1
% initialDomain(Domain) είναι αληθές αν έχουν οριστεί σαν κατηγορήματα τα frame και directions και επιστρέφει το αρχικό πεδίο τιμών για κάθε ντόμινο στην μορφή Domain = {(X,Y,Direction) | (Χ,Υ) έκγυρη θέση στο frame, Direction έγκυρη κατεύθυνση}. π.χ. (για frame([[1,2],[3,4]]). και directions([right, down, left, up]). ):
% ?- initialDomain((Domain)).
% Domain = [(1,1,right), (1,1,down), (1,1,left), (1,1,up), (1,2,right), (1,2,down), (1,2,left), (1,2,up), (2,1,right), (2,1,down), (2,1,left), (2,1,up), (2,2,right), (2,2,down), (2,2,left), (2,2,up)] 

posOfValue((X, Y, _), (X,Y)).
directionOfValue((_,_,Direction), Direction).
% -------------------------------------------------------------------------
%   FC + MRV Implementation

fc_dominos(Dominos) :-
    numOfDominos(N),
    length(Dominos, N),
    initialDomain(Domain),
    combine_soldom(Dominos, 1, Domain, SolDom),
    generate_solution(SolDom).

fm_dominos(Dominos) :-
    numOfDominos(N),
    length(Dominos, N),
    initialDomain(Domain),
    combine_soldom(Dominos, 1, Domain, SolDom),
    generate_solution_mrv(SolDom).

combine_soldom([], _, _, []).
combine_soldom([X|Dominos], I, Domain, [X-I-Domain|SolDom]) :-
   I1 is I + 1,
   combine_soldom(Dominos, I1, Domain, SolDom).

generate_solution_mrv([]).
generate_solution_mrv(SolDom1) :-
    mrv_var(SolDom1, X-I-Domain, SolDom2),
    domino(I, Domino),
    member(X, Domain),
    directionOfValue(X, Direction),
    posOfValue(X, Pos),
    properPos(Domino, Pos, Direction),
    update_domains(X, SolDom2, SolDom3),
    generate_solution(SolDom3).

generate_solution([]).
generate_solution([X-I-Domain|SolDom2]) :-
    % mrv_var(SolDom1, X-I-Domain, SolDom2),
    domino(I, Domino),
    member(X, Domain),
    directionOfValue(X, Direction),
    posOfValue(X, Pos),
    properPos(Domino, Pos, Direction),
    update_domains(X, SolDom2, SolDom3),
    generate_solution(SolDom3).

mrv_var([X-I-Domain], X-I-Domain, []).
mrv_var([X1-I1-Domain1|SolDom1], X-I-Domain, SolDom3) :-
   mrv_var(SolDom1, X2-I2-Domain2, SolDom2),
   length(Domain1, N1),
   length(Domain2, N2),
   (N1 < N2 ->
      (X = X1,
       I = I1,
       Domain = Domain1,
       SolDom3 = SolDom1) ;
      (X = X2,
       I = I2,
       Domain = Domain2,
       SolDom3 = [X1-I1-Domain1|SolDom2])).

update_domains(_, [], []).
update_domains(X, [Y-I2-Domain1|SolDom1], [Y-I2-Domain2|SolDom2]) :-
   update_domain(X, Domain1, Domain2),
   update_domains(X, SolDom1, SolDom2).

update_domain(X, Domain1, Domain3) :-
    posOfValue(X, Pos),
    remove_pos_if_exists(Pos, Domain1, Domain2),
    directionOfValue(X, Direction),
    posOfValueB(Pos, Direction, PosB),
    remove_pos_if_exists(PosB, Domain2, Domain3).

remove_pos_if_exists((X,Y), List, List4) :-
    remove_if_exists((X, Y, right), List, List1),
    remove_if_exists((X, Y, down), List1, List2),
    remove_if_exists((X, Y, left), List2, List3),
    remove_if_exists((X, Y, up), List3, List4).    

remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
   !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).

% -------------------------------------------------------------------------
%   More Helpers

posOfListItem(Item, List, Pos) :-
    posOfListItemAcc(Item, List, Pos, 1).

posOfListItemAcc(Item, [Item|_], Acc, Acc) :- !.
posOfListItemAcc(Item, [Item1|List], Pos, Acc) :-
    Item1 \= Item,
    Acc1 is Acc + 1,
    posOfListItemAcc(Item, List, Pos, Acc1).

variableTable(Table, Width, Height) :-
    length(Table, Height),
    constructRows(Table, Width).

constructRows([], _).
constructRows([X|Rows], Width) :-
    length(X, Width),
    constructRows(Rows, Width).

reverseDirection(right, left).
reverseDirection(left, right).
reverseDirection(down, up).
reverseDirection(up, down).

valueA((A, _), A).
valueB((_, B), B).

getValueDirect((X,Y), Dominos, Value) :-
    member((X,Y, Direction), Dominos),
    posOfListItem((X,Y,Direction), Dominos, Pos),
    domino(Pos, Domino),
    valueA(Domino, ValueA),
    directionToNote(Direction, Note),
    Value = (ValueA, Note).

getValueIndirect((X,Y), Dominos, Value) :-
    member(Direction, [down,up,left,right]),
    reverseDirection(Direction, ReverseDir),
    posOfValueB((X,Y), Direction, (XA, YA)),
    posOfListItem((XA,YA,ReverseDir), Dominos, Pos),
    domino(Pos, Domino),
    valueB(Domino, ValueB),
    directionToNote(Direction, Note),
    Value = (ValueB, Note).

getValue(Pos, Dominos, Value) :-
    getValueDirect(Pos, Dominos, Value) ;
    getValueIndirect(Pos, Dominos, Value).

directionToNote(right, horizontal).
directionToNote(down, vertical).
directionToNote(left, nothing).
directionToNote(up, nothing).

% --------------------------------------------------------------------
%   Printing

printDominos(Dominos) :-
    printTable(Dominos, 1).

printTable(_, Y) :-
    getFrameHeight(Height),
    Y is Height + 1,
    !.
printTable(Dominos, Y) :-
    getFrameHeight(Height),
    Y =< Height,
    printRowHorizontal(Y, 1, Dominos),
    write('\n'),
    printRowVertical(Y, 1, Dominos),
    write('\n'),
    Y1 is Y + 1,
    printTable(Dominos, Y1).

printRowHorizontal(_, X, _) :-
    getFrameWidth(Width),
    X > Width,
    !.
printRowHorizontal(Y, X, Dominos) :-
    getFrameWidth(Width),
    X =< Width,
    getValue((X,Y), Dominos, Value),
    printValueHorizontal(Value),
    X1 is X + 1,
    printRowHorizontal(Y, X1, Dominos).

printValueHorizontal((Value, Note)) :-
    write(Value),
    (
        (
            Note = horizontal,
            write('-')
        )
        ;
        (
            Note = vertical,
            write(' ')
        )
        ;
        (
            Note = nothing,
            write(' ')
        )
    ).

printRowVertical(_, X, _) :-
    getFrameWidth(Width),
    X > Width,
    !.
printRowVertical(Y, X, Dominos) :-
    getFrameWidth(Width),
    X =< Width,
    getValue((X,Y), Dominos, Value),
    printValueVertical(Value),
    X1 is X + 1,
    printRowVertical(Y, X1, Dominos).
    
printValueVertical((_, Note)) :-
    (
        (
            Note = horizontal,
            write(' ')
        )
        ;
        (
            Note = vertical,
            write('|')
        )
        ;
        (
            Note = nothing,
            write(' ')
        )
    ),
    write(' ').


ugly_print([],[]).
ugly_print([Y|Pos], [ X | Dominos]) :-
    write('Domino '),
    write(Y),
    write(': \tat '),
    write(X),
    write('\n'),
    ugly_print(Pos, Dominos).

% --------------------------------------------------------------------

put_dominos_fc_only :-
    fc_dominos(Dominos),
    printDominos(Dominos).

put_dominos :-
    fm_dominos(Dominos),
    printDominos(Dominos).

put_dominos_ugly_print :-
    fm_dominos(Dominos),
    getDominos(D),
    ugly_print(D, Dominos).
    