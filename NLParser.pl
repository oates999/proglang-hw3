main :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),
    close(Str),
    write(Words), nl.


% Credit to StackOverflow and author Ishq for file parser
% https://stackoverflow.com/a/4805931
% https://stackoverflow.com/users/577045/ishq
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

%Converts sentence to a list of words
lines_to_words([], []).
lines_to_words([H|T], [H2|T2]) :-
	split_string(H, " ", "", H2),
	lines_to_words(T, T2).