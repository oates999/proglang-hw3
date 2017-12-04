:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

main() :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),

    write(Words), nl,
    start(X,Y),
    check_sentences(Words, X, Y),

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


check_sentences([]).

check_sentences([S|Words], X, Y) :-
    write(S),
    ( sentence(S) -> (valid_move(S,X,Y,XNew,YNew) -> check_sentences(Words, XNew, YNew) ; write(" INVALID MOVE")) ; write(" INVALID SENTENCE"), nl, check_sentences(Words, X, Y) ), nl.


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
object("square").
object("squares").
object("cell").
object("cells").

direction("up").
direction("down").
direction("left").
direction("right").

up("up").
down("down").
left("left").
right("right").


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



% "he pushed the button"
valid_move([W, X, Y, Z], RatX, RatY, RatXNew, RatYNew) :-
    subject_phrase(W),
    verb_phrase(X, Y, Z),
    RatXNew is RatX,
    RatYNew is RatY,
    write("  BUTTON PUSH"), nl.

% "the rat pushed the button"
valid_move([V, W, X, Y, Z], RatX, RatY, RatXNew, RatYNew) :-
    subject_phrase(V, W),
    verb_phrase(X, Y, Z),
    RatXNew is RatX,
    RatYNew is RatY,
    write("  BUTTON PUSH"), nl.

% "he moved 2 squares up"
valid_move([V, W, X, Y, Z], RatX, RatY, RatXNew, RatYNew) :-
    subject_phrase(V),
    verb_phrase(W, X, Y, Z),

    number_codes(N, X),

    write("  MOVE "),
    write(N), nl,
    attempt_walk(Z, N, RatX, RatY, RatXNew, RatYNew).

% "the rat moved 2 squares up"
valid_move([U, V, W, X, Y, Z], RatX, RatY, RatXNew, RatYNew) :-
    subject_phrase(U, V),
    verb_phrase(W, X, Y, Z),

    number_codes(N, X),
    
    write("  MOVE "), 
    write(N), nl,

    attempt_walk(Z, N, RatX, RatY, RatXNew, RatYNew).
    


attempt_walk(Direction, Number, RatX, RatY, RatXNew, RatYNew) :-
    (   up(Direction) ->
        writeln("UP"),
        RatY - Number >= 0,
        can_walk_up(Number, RatX, RatY),
        RatXNew is RatX,
        RatYNew is RatY-Number
        
    ;   down(Direction) ->
        writeln("DOWN"),
        info(_, Height, _),
        RatY + Number < Height,
        can_walk_down(Number, RatX, RatY),
        RatXNew is RatX,
        RatYNew is RatY+Number

    ;   left(Direction) ->
        writeln("LEFT"),
        RatX - Number >= 0,
        can_walk_left(Number, RatX, RatY),
        RatXNew is RatX-Number,
        RatYNew is RatY

    ;   right(Direction) ->
        writeln("RIGHT"),
        info(Width, _, _),
        RatX + Number < Width,
        can_walk_right(Number, RatX, RatY),
        RatXNew is RatX+Number,
        RatYNew is RatY
    ),
    nl.


can_walk_up(0, PosX, PosY).

can_walk_up(N, PosX, PosY) :-
    NewN is N-1,
    NewX is PosX,
    NewY is PosY-1,
    format("checking ~d, ~d    ~d", [NewX, NewY, NewN]), nl,
    \+ wall(NewX, NewY),
    can_walk_up(NewN, NewX, NewY).



can_walk_down(0, PosX, PosY).

can_walk_down(N, PosX, PosY) :-
    NewN is N-1,
    NewX is PosX,
    NewY is PosY+1,
    format("checking ~d, ~d    ~d", [NewX, NewY, NewN]), nl,
    \+ wall(NewX, NewY),
    can_walk_down(NewN, NewX, NewY).



can_walk_left(0, PosX, PosY).

can_walk_left(N, PosX, PosY) :-
    NewN is N-1,
    NewX is PosX-1,
    NewY is PosY,
    format("checking ~d, ~d    ~d", [NewX, NewY, NewN]), nl,
    \+ wall(NewX, NewY),
    can_walk_left(NewN, NewX, NewY).



can_walk_right(0, PosX, PosY).

can_walk_right(N, PosX, PosY) :-
    NewN is N-1,
    NewX is PosX+1,
    NewY is PosY,
    format("checking ~d, ~d    ~d", [NewX, NewY, NewN]), nl,
    \+ wall(NewX, NewY),
    can_walk_right(NewN, NewX, NewY).