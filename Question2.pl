:- dynamic board/3.

initialize(Board):-
    retractall(board(_,_,_)),
    assert(Board).

boardSize(Rows, Cloumns):-
    board(Rows, Columns, _).

startPosition(Row, Column , Color):-
    start(Row, Column, Color).

goalPosition(Row, Column, Color):-
    goal(Row, Column, Color).

setBoard(Rows, Columns, Board):-
    assert(board(Rows, Columns, Board)).

setStartPosition(Row,Column, Color):-
    retractall(start(_, _, _)),
    assert(start(Row, Column, Color)).

setGoalPosition(Row, Column, Color):-
    retractall(goal(_, _, _)),
    assert(goal(Row, Column, Color)).

initialState(CurrentRow, CurrentCol, GoalRow, GoalColumn, Path):-
    start(CurrentRow, CurrentCol, _),
    goal(GoalRow, GoalCol, _),
    Path = [(CurrentRow, CurrentCol)].

move([X, Y], [X], Y], 1):-
    X1 is X + 1,
    board(Rows, _, _),
    X1 < Rows,
    vaildColor(X1, Y).
move([X, Y], [X, Y1], 1):-
    Y1 is Y + 1,
    Y1 >= 0,
    validColor(X, Y1).
validColor(X, Y):-
    board(_, _, Board),
    nth(X, Board, Row),   %complete nth0


search:-
    initialState(CurrentRow, CurrentCol, GoalRow, GoalCol,

getAllValidChildren(Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Node, Open, Closed, Goal, Next), Children).

getNextState([State, _, G, _, _], Open, Closed, Goal, [Next, State, NewG,
    move(State, Next, MoveCost),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
   (not(member({Next, _, _, _, _], Open));memberButBetter(Next, Open, NewF)),
   (not(member([Next, _, _, _, _], Closed));memberButBetter(Next, Closed, NewF)).


memberButBetter(Next, List, NewF):-
    findall(F, member,([Next, _, _, _, F), List), Numbers),
    minList(Numbers, MinOldF),
    MinOldF > NewF.

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).
    getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

%to print the path solution
printSolution([State, null, G, H,F], _):-
    write([State, G, H,F]), nl.
printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    PrintSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(State, G, H, F]), nl.
