main(Index) :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),

    nth0(Index, Words, Sent),

    write(Words), nl,
    write(Sent), nl,
    sentence(Sent),
    close(Str).

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





article("the").
article("a").

subject("einstein").
subject("rat").
subject("rodent").
subject("he").
subject("it").

verb("moved").
verb("ran").
verb("scurried").
verb("pushed").

object("button").
object("squares").
object("cell").

direction("up").
direction("down").
direction("left").
direction("right").


subject_phrase(X, Y) :-
    article(X),
    subject(Y).

subject_phrase(X) :-
    subject(X).

object_phrase(X, Y) :-
    article(X),
    object(Y).

direction_object_phrase(X, Y, Z) :-
    number_codes(N, X),
    integer(N),
    object(Y),
    direction(Z).


verb_phrase(X, Y, Z) :-
    verb(X),
    object_phrase(Y, Z).

verb_phrase(W, X, Y, Z) :-
    verb(W),
    direction_object_phrase(X, Y, Z).


sentence([W, X, Y, Z]) :-
    subject_phrase(W),
    verb_phrase(X, Y, Z).

sentence([V, W, X, Y, Z]) :-
    subject_phrase(V, W),
    verb_phrase(X, Y, Z).

sentence([V, W, X, Y, Z]) :-
    subject_phrase(V),
    verb_phrase(W, X, Y, Z).

sentence([U, V, W, X, Y, Z]) :-
    subject_phrase(U, V),
    verb_phrase(W, X, Y, Z).
