startGame:-
write("Do you want x or o?"), nl,
read(Human),
% Human player can be x or o but x is always MAX and o is MIN
StartState = ['#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
play([Human, StartState], Human).

otherPlayer(x,o).

otherPlayer(o,x).

play([_, State], _):-
isTerminal(State), !,
nl, draw(State), nl.

play([Human, State], Human):-
!, nl, draw(State), nl,
write('Enter your move\'s index'),nl,
read(HumanMove),
% Check that the move is valid
% then replace in the board(using select & insert)
nth0(HumanMove, State, '#', TmpList),
nth0(HumanMove, NextState, Human, TmpList),
otherPlayer(Human, Computer),
play([Computer, NextState], Human).

play([Computer, State], Human):-
nl, draw(State), nl,
computerTurn([Computer,State], NextState),
play(NextState, Human).

computerTurn(State, Next):-
alphabeta(State, -100, 100, Next, _).

isTerminal(State):-
getWinner(State, Winner), write(Winner), write(' wins!'), nl, !.

isTerminal(State):-
not(member('-', State)), write('It\'s a draw!'), nl.

getWinner(State, Winner):-
check_sequence(State, Z), Winner = Z.
	 
check_sequence(List, Z) :-
    % view the list as a 5x5 matrix
    length(List, 25),
    List = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y],
    Matrix = [[A,B,C,D,E], [F,G,H,I,J], [K,L,M,N,O], [P,Q,R,S,T], [U,V,W,X,Y]],
    % check for horizontal sequences
    (((member([Z,Z,Z,Z,_], Matrix); member([_,Z,Z,Z,Z], Matrix)), Z \= '#');
    % check for vertical sequences
    (vertical(Matrix, Z));
    % check for diagonal sequences
    (diagonal(Matrix, Z))).
	
vertical(Lists, Z) :-
    length(Lists, Len),
    Len = 5,
    nth0(0, Lists, L1),
	nth0(1, Lists, L2),
	nth0(2, Lists, L3),
	nth0(3, Lists, L4),
	nth0(4, Lists, L5),
	((nth0(Index, L1, Z),
	nth0(Index, L2, Z),
	nth0(Index, L3, Z),
	nth0(Index, L4, Z));
	(nth0(Index, L5, Z),
	nth0(Index, L2, Z),
	nth0(Index, L3, Z),
	nth0(Index, L4, Z))), Z \= '#'.
	
diagonal(Lists, Z) :-
	length(Lists, Len),
    Len = 5,
    nth0(0, Lists, L1),
	nth0(1, Lists, L2),
	nth0(2, Lists, L3),
	nth0(3, Lists, L4),
	nth0(4, Lists, L5),
	((nth0(0, L1, Z),
	nth0(1, L2, Z),
	nth0(2, L3, Z),
	nth0(3, L4, Z));
	(nth0(1, L2, Z),
	nth0(2, L3, Z),
	nth0(3, L4, Z),
	nth0(4, L5, Z));
	(nth0(0, L2, Z),
	nth0(1, L3, Z),
	nth0(2, L4, Z),
	nth0(3, L5, Z));
	(nth0(1, L1, Z),
	nth0(2, L2, Z),
	nth0(3, L3, Z),
	nth0(4, L4, Z));
	(nth0(4, L1, Z),
	nth0(3, L2, Z),
	nth0(2, L3, Z),
	nth0(1, L4, Z));
	(nth0(3, L1, Z),
	nth0(2, L2, Z),
	nth0(1, L3, Z),
	nth0(0, L4, Z));
	(nth0(4, L2, Z),
	nth0(3, L3, Z),
	nth0(2, L4, Z),
	nth0(1, L5, Z));
	(nth0(3, L2, Z),
	nth0(2, L3, Z),
	nth0(1, L4, Z),
	nth0(0, L5, Z))), Z \= '#'.
	
draw(List) :-
    % view the list as a 5x5 matrix
    length(List, 25),
    List = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y],
    Matrix = [[A,B,C,D,E], [F,G,H,I,J], [K,L,M,N,O], [P,Q,R,S,T], [U,V,W,X,Y]],
    % write the matrix row by row
    write('------------------------'), nl,
    write_row(Matrix, 1).

write_row([], _).
write_row([Row|Rest], N) :-
    write(' | '),
    write_elements(Row), nl,
    write(' ------------------------'), nl,
    N1 is N + 1,
    write_row(Rest, N1).

write_elements([]).
write_elements([X|Xs]) :-
    write(' '),
    write(X),
    write(' |'),
    write_elements(Xs).


alphabeta(Pos, Alpha, Beta, BestNextPos, Val):-
bagof(NextPos, move(Pos, NextPos), NextPosList),
best(NextPosList, Alpha, Beta, BestNextPos, Val), !.
alphabeta(Pos, _, _,_, Val):-
utility(Pos, Val).
best([Pos|_],Alpha, Beta, _, Val):-
Beta =< Alpha, !,
(isMinPlayer(Pos) -> Val is Alpha ; Val is Beta).

best([Pos], Alpha, Beta, Pos, Val):-
alphabeta(Pos, Alpha, Beta, _, Val), !.
best([Pos1 | Tail], Alpha, Beta, BestPos, BestVal) :-
alphabeta(Pos1, Alpha, Beta, _, Val1),
updateValues(Pos1, Val1, Alpha, Beta, NewAlpha, NewBeta),
best(Tail, NewAlpha, NewBeta, Pos2, Val2),
betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).
updateValues(Pos1, Value, Alpha, Beta, NewAlpha, Beta):-
isMinPlayer(Pos1), !,
(Value > Alpha -> (NewAlpha is Value, !)
; NewAlpha is Alpha

).
updateValues(_, Value, Alpha, Beta, Alpha, NewBeta):-
(Value < Beta -> (NewBeta is Value, !)
; NewBeta is Beta

).
betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
isMinPlayer(Pos1),
(Val1 >= Val2 -> (BestPos = Pos1, BestVal is Val1, !)
; (BestPos = Pos2, BestVal is Val2)

), !.
betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
(Val1 =< Val2 -> (BestPos = Pos1, BestVal is Val1, !)
; (BestPos = Pos2, BestVal is Val2)

), !.

move([Player, State], Next):-
not(getWinner(State,_)),
getMove([Player, State], Next).

getMove([Player, State], [NextPlayer, NextState]):-
otherPlayer(Player, NextPlayer),
State = ['#'|T],
NextState = [Player|T].

getMove([Player, State], [NextPlayer, NextState]):-
State = [H|T],
NextState = [H|NextT],
move([Player,T], [NextPlayer, NextT]).

utility([_,State], Val):-
getWinner(State, Winner),!,
((Winner = x, Val = 1, !);
(Winner = o, Val = -1, !)).

utility(_,0).
isMinPlayer([o,_]). % o is the next player