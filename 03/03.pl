:- use_module(library(lists)).

score(CharCode, Score) :-
    between(65, 90, CharCode),
    Score is CharCode - 38.

score(CharCode, Score) :-
    between(97, 122, CharCode),
    Score is CharCode - 96.

part1(Lines, Total) :-
    maplist(part1_score_line, Lines, Scores),
    sumlist(Scores, Total).

part1_score_line(Chars, Score) :-
    same_length(First, Second),
    append(First, Second, Chars),
    intersection(First, Second, [Char|_]),
    score(Char, Score).

chunk(_, [], []).
chunk(N, List, [SubList|SubLists]) :-
    length(SubList, N),
    append(SubList, Rest, List),
    chunk(N, Rest, SubLists).

part2(Lines, Total) :-
    chunk(3, Lines, Chunked),
    maplist(part2_score_chunk, Chunked, Scores),
    sumlist(Scores, Total).

part2_score_chunk([A, B, C], Score) :-
    list_to_set(A, SetA),
    list_to_set(B, SetB),
    list_to_set(C, SetC),
    intersection(SetA, SetB, X),
    intersection(X, SetC, [Char]),
    score(Char, Score).

read_input(Lines) :-
    open("input_full.txt", read, Input),
    read_file_to_list(Input, StringLines),
    maplist([Line, Chars]>>string_codes(Line, Chars), StringLines, Lines).

main :-
    read_input(Lines),
    part1(Lines, Part1Total),
    print(Part1Total), nl,
    part2(Lines, Part2Total),
    print(Part2Total), nl,
    halt.

read_file_to_list(Stream, []) :-
    at_end_of_stream(Stream).

read_file_to_list(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_file_to_list(Stream, L).
