:- use_module(library(lists)).
:- use_module(library(readutil)).

move(rock).
move(paper).
move(scissors).

result(loss).
result(draw).
result(win).

play(A, A, draw).
play(rock, paper, loss).
play(rock, scissors, win).
play(paper, scissors, loss).
play(paper, rock, win).
play(scissors, rock, loss).
play(scissors, paper, win).

value(rock, 1).
value(paper, 2).
value(scissors, 3).
value(loss, 0).
value(draw, 3).
value(win, 6).

score(OpponentMove, Move, Result, Score) :-
    play(Move, OpponentMove, Result),
    value(Move, MoveScore),
    value(Result, RoundScore),
    Score is MoveScore + RoundScore.

part1_solve(List, TotalScore) :-
    maplist(part1_score, List, Scores),
    sumlist(Scores, TotalScore).

part1_score([OpponentMove, Move, _], Score) :-
    score(OpponentMove, Move, _, Score).

part2_solve(List, TotalScore) :-
    maplist(part2_score, List, Scores),
    sumlist(Scores, TotalScore).

part2_score([OpponentMove, _, Result], Score) :-
    score(OpponentMove, _, Result, Score).

parse(_, "A", rock).
parse(_, "B", paper).
parse(_, "C", scissors).
parse(part1, "X", rock).
parse(part1, "Y", paper).
parse(part1, "Z", scissors).
parse(part2, "X", loss).
parse(part2, "Y", draw).
parse(part2, "Z", win).

read_input(List) :-
    open("input_full.txt", read, Input),
    read_file_to_list(Input, Lines),
    maplist(parse_line, Lines, List).

parse_line(Line, [OpponentMove, Move, Result]) :-
    split_string(Line, " ", "", [L, R]),
    parse(_, L, OpponentMove),
    parse(part1, R, Move),
    parse(part2, R, Result).

main :-
    read_input(List),
    part1_solve(List, Part1Total),
    part2_solve(List, Part2Total),
    print(Part1Total), nl,
    print(Part2Total), nl,
    halt.

read_file_to_list(Stream, []) :-
    at_end_of_stream(Stream).

read_file_to_list(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_file_to_list(Stream, L).
