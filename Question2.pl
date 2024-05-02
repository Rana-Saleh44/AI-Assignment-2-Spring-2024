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






printSolution([State, null, G, H,F], _):-
    write([State, G, H,F]), nl.
printSolution([State, Parent, G, H, F], Closed),

