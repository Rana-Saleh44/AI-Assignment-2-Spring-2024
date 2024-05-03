
:- dynamic board/3.

initialize(Board):-
    retractall(board(,,_)),
    assert(Board).

boardSize(Rows, Columns):-
    board(Rows, Columns, _).

startPosition(Row, Column, Color):-
    start(Row, Column, Color).

goalPosition(Row, Column, Color):-
    goal(Row, Column, Color).

setBoard(Rows, Columns, Board):-
    assert(board(Rows, Columns, Board)).

setStartPosition(Row, Column, Color):-
    retractall(start(_, _, _)),
    assert(start(Row, Column, Color)).

setGoalPosition(Row, Column, Color):-
    retractall(goal(_, _, _)),
    assert(goal(Row, Column, Color)).

initialState(CurrentRow, CurrentCol, GoalRow, GoalCol, Path):-
    start(CurrentRow, CurrentCol, _),
    goal(GoalRow, GoalCol, _),
    Path = [(CurrentRow, CurrentCol)].

move([X, Y], [X1, Y], 1):-
    X1 is X + 1,
    board(Rows, _, _),
    X1 < Rows,
    validColor(X1, Y).
move([X, Y], [X, Y1], 1):-
    Y1 is Y + 1,
    Y1 >= 0,
    validColor(X, Y1).

validColor(X, Y):-
    board(_, _, Board),
    nth(X, Board, Row),
    nth(Y, Row, red).

calculateH([X1, Y1], [X2, Y2], H):-
    H is abs(X1 - X2) + abs(Y1 - Y2).

findPath([GoalRow, GoalCol], [GoalRow, GoalCol], [GoalRow, GoalCol]).

findPath([CurrentRow, CurrentCol], [GoalRow, GoalCol], [[CurrentRow, CurrentCol]|Path]):-
    move([CurrentRow, CurrentCol], NextPosition, _),   % To get the next valid move
    findPath(NextPosition, [GoalRow, GoalCol], Path).



start_game(Board, startRow, startCol, GoalRow, GoalCol):-
    initialize(Board),
    search([[[startRow, startCol], null, 0, 0, 0]], [], [GoalRow, GoalCol]).

search:-
    initialState(CurrentRow, CurrentCol, GoalRow, GoalCol, _),
    initPath([CurrentRow, CurrentCol], [GoalRow, GoalCol], Path),
    write('Found path: '), write(Path), nl,!.

search(Open, Closed, Goal):-
    getBestState(Open, [CurrentState, Parent, G, F, H], _),
    CurrentState = Goal,
    write("Search is Complete"), nl,
    printSolution([CurrentState, Parent, G, H, F], Closed), !.

search(Open, Closed, Goal):-
    getBestState(Open, CurrentNode, TempOpen),
    getAllValidChildren(CurrentNode, TempOpen, Closed, Goal, Children),
    addChildren(Children, TempOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed),
    search(NewOpen, NewClosed, Goal).

findMin([X], X):- !.

findMin([H|T], Min):-
    findMin(T, TempMin),
    H = [,,_,H, HF],
    TempMin = [,,_,TempH,TempF],
    (TempF < HF -> Min = TempMin; Min = Head).

getAllValidChildren(Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Node, Open, Closed, Goal, Next), Children).

getNextState([State, _, G, _, _], Open, Closed, Goal, [Next, State, NewG]):-
    move(State, Next, MoveCost),
    calculateH(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
   ( not(member([Next, _, _, _, _], Open));memberButBetter(Next, Open, NewF)),
   ( not(member([Next, _, _, _, _], Closed));memberButBetter(Next, Closed, NewF)).

memberButBetter(Next, List, NewF):-
    findall(F, (member([Next, _, _, _, F], List)), Numbers),
    minList(Numbers, MinOldF),
    MinOldF > NewF.

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

% Print the path solution
printSolution([State, null, G, H, F], _):-
    write([State, G, H, F]), nl.
printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(State, G, H, F), nl.
