read_color(Color):-
    write('Enter Cell Color'),
    read(Color).

init_list(Size, List):-
    length(List, Size),
    maplist(read_color, List).

init_board(Rows, Cols, Board):-
    length(Board, Rows),
    maplist(init_list(Cols), Board).

% bfs to find a cycle
bfs_find_cycle(Board, Cycle):-
    findall(cell(R, C, Color), (nth0(R, Board, Row), (nth0(C, Row, Color)), Cells),
   bfs([[Cell]], Cells, [], Cycle)).

bfs([], _, _, []):- !.

bfs([Path|_], _, _, Path):-
    length(Path, L),
    L >= 4,
    Path = [cell(R, C, Color)|Rest],
    last(Path, cell(LR, LC, Color)),
    adjacent_cells(LR, LC, R, C), !.

bfs([Path|Paths], Cells, Visited, NewPaths):-

    extend_path(Path, Cells, Visited, NewPaths),
    append(Paths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, Cells, [Path|Visited], Cycle).

extend_path([cell(R, C, Color)|T], Cells, Visited, NewPaths):-
    findall([cell(NR, NC, Color),cell(R, C, Color)|T],
    (   adjacent_cells(R, C, NR, NC),
        member(cell(NR, NC, Color), Cells),
        \+ member(cell(NR, NC, Color), T)
    ),NewPaths).

adjacent_cells(R, C, NR, NC):-
    board_size(MaxR, MaxC),
    member((NR, NC), [(R+1, C), (R-1, C), (R, C+1), (R, C-1)]),
    NR >= 0, NR < MaxR, NC >= 0, NC < MaxC.

:- dynamic board_size/2.

main :-
    write('Enter number of rows: '),
    read(R),
    write('Enter number of columns: '),
    read(C),
    assert(board_size(R, C)),
    (   R < 2, C < 2
    ->  write('No Cycle'),nl
    ;   init_board(R, C, Board),
        bfs_find_cycle(Board, Cycle),
        (   Cycle = [] -> write('No found Cycle'), nl
        ;   write('Cycle found: '), write(Cycle), nl
        )
    ).
