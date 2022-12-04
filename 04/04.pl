:- use_module(library(lists)).

range(X, X, [X]).
range(X, Y, [X|Xs]) :- Z is X+1, range(Z, Y, Xs).

summaplist(F, List, Sum) :- maplist(F, List, Results), sumlist(Results, Sum).

full_overlap([List1, List2], 1) :- intersection(List1, List2, List1).
full_overlap([List1, List2], 1) :- intersection(List1, List2, List2).
full_overlap(_, 0).

partial_overlap([List1, List2], 1) :- intersection(List1, List2, [_|_]).
partial_overlap(_, 0).

parse_line(String, [Assignment1, Assignment2]) :-
    split_string(String, ",", "", Parts),
    maplist(parse_assignment, Parts, [Assignment1, Assignment2]).

parse_assignment(String, Range) :-
    split_string(String, "-", "", Parts),
    maplist(atom_number, Parts, [Low, High]),
    range(Low, High, Range).

read_input(Lines) :-
    open("input_full.txt", read, Input),
    read_file_to_list(Input, StringLines),
    maplist(parse_line, StringLines, Lines).

main :-
    read_input(Lines),
    summaplist(full_overlap, Lines, Part1Total),
    print(Part1Total), nl,
    summaplist(partial_overlap, Lines, Part2Total),
    print(Part2Total), nl,
    halt.

read_file_to_list(Stream, []) :- at_end_of_stream(Stream).
read_file_to_list(Stream, [Line|Lines]) :-
    read_line_to_string(Stream, Line),
    read_file_to_list(Stream, Lines).
