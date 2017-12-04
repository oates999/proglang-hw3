:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).

main() :-
	open('path-solution.txt',write,Stream),
	write(Stream,""),
	close(Stream),
	start(WhatX,WhatY),
	append_to_path(WhatX,WhatY,[],NewPath),
	move(WhatX,WhatY,1,NewPath).


% "Helper function to determine membership in a list"
member(X,[X|T]).
member(X,[H|T]) :- member(X,T).

% "Helper function to append two lists"
append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).


% "True if (X,Y) is a valid floor tile"
test_location(X,Y,GoalNumber,Path) :-
	inside_maze_bounds(X,Y),
	not_wall(X,Y),
	not_invalid_button(X,Y,GoalNumber),
	not_visited(X,Y,Path).

% "True if (X,Y) is within the bounds of the maze"
inside_maze_bounds(X,Y) :-
	info(WhatX,WhatY,WhatC),
	X < WhatX, X >= 0,
	Y < WhatY, Y >= 0.

% "True is wall(X,Y) is not defined"
not_wall(X,Y) :-
	\+ wall(X,Y).

% "True if (X,Y) is not a button"
not_invalid_button(X,Y,GoalNumber) :-
	\+ button(X,Y,WhatButton).
% "True if (X,Y) is a button that has already been pressed"
not_invalid_button(X,Y,GoalNumber) :-
	button(X,Y,WhatButton),
	GoalNumber >= WhatButton.

% "Creates path entry for (X,Y) and checks if entry is a member of the path"
not_visited(X,Y,Path) :-
	coordinates_to_string(X,Y,CoordinateString),
	\+ member(CoordinateString,Path).


% "Testing/Relocation to Right tile"
move(X,Y,GoalNumber,Path) :-
	NewX is X+1,
	test_location(NewX,Y,GoalNumber,Path),
	relocate(NewX,Y,GoalNumber,Path).
% "Testing/Relocation to Left tile"
move(X,Y,GoalNumber,Path) :-
	NewX is X-1,
	test_location(NewX,Y,GoalNumber,Path),
	relocate(NewX,Y,GoalNumber,Path).
% "Testing/Relocation to Down tile"
move(X,Y,GoalNumber,Path) :-
	NewY is Y+1,
	test_location(X,NewY,GoalNumber,Path),
	relocate(X,NewY,GoalNumber,Path).
% "Testing/Relocation to Up tile"
move(X,Y,GoalNumber,Path) :-
	NewY is Y-1,
	test_location(X,NewY,GoalNumber,Path),
	relocate(X,NewY,GoalNumber,Path).

% "Behavior if goal has been reached and there are still buttons or final goal remaining"
relocate(X,Y,GoalNumber,Path) :-
	goal_reached(X,Y,GoalNumber),
	%%  write("button reached:  "),
	%%  coordinates_to_string(X,Y,CoordinateString),
	%%  write(CoordinateString),
	write_path_to_file(Path),
	NewGoalNumber is GoalNumber+1,
	coordinates_to_string(X,Y,CurrentCoordinates),
	move(X,Y,NewGoalNumber,[CurrentCoordinates]).
% "Behavior if final goal has been reached"
relocate(X,Y,GoalNumber,Path) :-
	final_goal_reached(X,Y,GoalNumber),
	%%  write("goal reached:  "),
	%%  coordinates_to_string(X,Y,CoordinateString),
	%%  write(CoordinateString),
	append_to_path(X,Y,Path,NewPath),
	write_path_to_file(NewPath).
% "Behavior if no goal has been reached"
relocate(X,Y,GoalNumber,Path) :-
	\+ goal_reached(X,Y,GoalNumber),
	%%  coordinates_to_string(X,Y,CoordinateString),
	%%  write(CoordinateString),
	append_to_path(X,Y,Path,NewPath),
	move(X,Y,GoalNumber,NewPath).

% "If all buttons have already been reached, compare to overall goal"
final_goal_reached(X,Y,GoalNumber) :-
	num_buttons(WhatN),
	GoalNumber > WhatN,
	goal(X,Y).

% "If not all buttons have been reached, compare to current button"
goal_reached(X,Y,GoalNumber) :-
	num_buttons(WhatN),
	GoalNumber =< WhatN,
	button(X,Y,GoalNumber).

% "Appends current location to the end of the path"
append_to_path(X,Y,Path,NewPath) :-
	coordinates_to_string(X,Y,CoordinateString),
	append(Path,[CoordinateString],NewPath).
% "Helper function to create a string from (X,Y) coordinates"
coordinates_to_string(X,Y,CoordinateString) :-
	atomic_list_concat([X,Y],',',Atom), atom_string(Atom,String),
	atomic_list_concat(["[",String,"]\n"],'',Atom2), atom_string(Atom2,CoordinateString).

% "Writes the entries of the path to path-solution.txt"
write_path_to_file([]).
write_path_to_file([H|T]) :-
	open('path-solution.txt',append,Stream),
	write(Stream,H),
	close(Stream),
	write_path_to_file(T).