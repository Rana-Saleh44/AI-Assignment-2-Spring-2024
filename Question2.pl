:- dynamic board/3.

initialize(Board):-
    retractall(board(_,_,_)),
    assert(Board).

board_size(Rows, Cloumns):-
    board(Rows, Columns, _).

start_position(Row, Column , Color):-
    start(Row, Column, Color).

goal_position(Row, Column, Color):-
    goal(Row, Column, Color).

set_board(Rows, Columns, Board):-
    assert(board(Rows, Columns, Board)).

set_start_position(Row,Column, Color):-
    retractall(start(_, _, _)),
    assert(start(Row, Column, Color)).

set_goal_position(Row, Column, Color):-
    retractall(goal(_, _, _)),
    assert(goal(Row, Column, Color)).


