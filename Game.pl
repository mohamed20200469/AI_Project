:- dynamic difficulty/1.
:- dynamic strategy/1.

startGame:-
write("Do you want r or b?"), nl,
read(Human),
%write("Select the difficulty of the A.I. (1 to 6)"), nl,
%read(Diff),
%write("Select the Strategy of the A.I. (1 for minmax or 2 for alphabeta)"), nl,
%read(Strat),
%assertz(difficulty(Diff)),
%assertz(strategy(Strat)),
% Human player can be r or b but r is always MAX and b is MIN
StartState = ['#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#'],
play([Human, StartState], Human).

otherPlayer(r,b).

otherPlayer(b,r).

play([_, State], _):-
isTerminal(State), !,
nl, draw(State), nl.

play([Human, State], Human):-
!, nl, draw(State), nl,
write('Enter your move\'s column'),nl,
read(HumanMove),
insert(State, HumanMove, Human, NextState),
otherPlayer(Human, Computer),
play([Computer, NextState], Human).

play([Computer, State], Human):-
    nl, draw(State), nl,
	write("A.I. turn"), nl,
    computerTurn([Computer,State], NextState),
    play(NextState, Human).

	
computerTurn(Pos, Next):-
	difficulty(Diff),
	strategy(Strat),
    (
		Strat == 2 -> alphabeta(Pos, -1000, 1000, Next, _, Diff);
		Strat == 1 ->  minimax(Pos, Next, _, Diff)
	).

isTerminal(State):-
getWinner(State, Winner), write(Winner), write(' wins!'), nl, !.

isTerminal(State):-
not(member('#', State)), write('It\'s a draw!'), nl.

getWinner(State, Winner):-
check_sequence(State, Z), Winner = Z.

alphabeta(Pos, Alpha, Beta, BestNextPos, Val, DepthLimit):-
    DepthLimit =< 0, !,
    utility(Pos, Val).

alphabeta(Pos, Alpha, Beta, BestNextPos, Val, DepthLimit):-
    DepthLimit > 0,
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    NewDepthLimit is DepthLimit - 1,
    best(NextPosList, Alpha, Beta, BestNextPos, Val, NewDepthLimit), !.

best([Pos|_], Alpha, Beta, _, Val, DepthLimit):-
    Beta =< Alpha, !,
    (isMinPlayer(Pos) -> Val is Alpha ; Val is Beta).

best([Pos], Alpha, Beta, Pos, Val, DepthLimit):-
    alphabeta(Pos, Alpha, Beta, _, Val, DepthLimit), !.

best([Pos1 | Tail], Alpha, Beta, BestPos, BestVal, DepthLimit) :-
    alphabeta(Pos1, Alpha, Beta, _, Val1, DepthLimit),
    updateValues(Pos1, Val1, Alpha, Beta, NewAlpha, NewBeta),
    best(Tail, NewAlpha, NewBeta, Pos2, Val2, DepthLimit),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

updateValues(Pos1, Value, Alpha, Beta, NewAlpha, Beta):-
    isMinPlayer(Pos1), !,
    (Value > Alpha -> (NewAlpha is Value, !)
    ; NewAlpha is Alpha).

updateValues(_, Value, Alpha, Beta, Alpha, NewBeta):-
    (Value < Beta -> (NewBeta is Value, !)
    ; NewBeta is Beta).

betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
    isMinPlayer(Pos1),
    (Val1 >= Val2 -> (BestPos = Pos1, BestVal is Val1, !)
    ; (BestPos = Pos2, BestVal is Val2)), !.

betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
    (Val1 =< Val2 -> (BestPos = Pos1, BestVal is Val1, !)
    ; (BestPos = Pos2, BestVal is Val2)), !.

move([Player, State], Next):-
    getMove([Player, State], Next).

getMove([Player, State], [NextPlayer, NextState]):-
    otherPlayer(Player, NextPlayer),
    nth0(Index, State, '#'),
    Column is Index mod 5,
    insert(State, Column, Player, NextState).

getMove([Player, State], [NextPlayer, NextState]):-
    State = [H|T],
    NextState = [H|NextT],
    move([Player,T], [NextPlayer, NextT]).

utility([_,State], Val):-
    getWinner(State, Winner),!,
    ((Winner = r, Val = 1, !);
    (Winner = b, Val = -1, !)).

utility(_,0).

isMinPlayer([b,_]). % b is the next player

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

insert(List, Column, Player, NewList):-
	length(StartState, 25),
	List = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y],
    Matrix = [[A,B,C,D,E], [F,G,H,I,J], [K,L,M,N,O], [P,Q,R,S,T], [U,V,W,X,Y]],
    nth0(0, Matrix, R1),
	nth0(1, Matrix, R2),
	nth0(2, Matrix, R3),
	nth0(3, Matrix, R4),
	nth0(4, Matrix, R5),
	((nth0(Column, R5, '#') -> replace_hash(R5, Column, Player, NewR5),  NewMatrix = [R1, R2, R3, R4, NewR5]);
	(nth0(Column, R4, '#') -> replace_hash(R4, Column, Player, NewR4),  NewMatrix = [R1, R2, R3, NewR4, R5]);
	(nth0(Column, R3, '#') -> replace_hash(R3, Column, Player, NewR3),  NewMatrix = [R1, R2, NewR3, R4, R5]);
	(nth0(Column, R2, '#') -> replace_hash(R2, Column, Player, NewR2),  NewMatrix = [R1, NewR2, R3, R4, R5]);
	(nth0(Column, R1, '#') -> replace_hash(R1, Column, Player, NewR1),  NewMatrix = [NewR1, R2, R3, R4, R5] ; write("Insertion failed, full column"))),
	flatten_matrix(NewMatrix, NewList).
	
% replace_hash(+List, +Index, +NewValue, -NewList)
% replaces the element in `List` at index `Index` with `NewValue` if it is `#`,
% and returns the resulting `NewList`. If the element is not `#`, the predicate fails.
replace_hash(List, Index, NewValue, NewList) :-
    nth0(Index, List, '#'),
    replace_nth(Index, List, NewValue, NewList).

% replace_nth(+Index, +List, +NewValue, -NewList)
% replaces the element in `List` at index `Index` with `NewValue`, and returns the
% resulting `NewList`.
replace_nth(0, [_|Rest], Element, [Element|Rest]) :- !.
replace_nth(Index, [First|Rest], Element, [First|NewRest]) :-
    Index > 0,
    NextIndex is Index - 1,
    replace_nth(NextIndex, Rest, Element, NewRest).


% flatten_matrix(+Matrix, -FlatList)
% flattens the 5x5 matrix `Matrix` into a 25-element list `FlatList`.
flatten_matrix(Matrix, FlatList) :-
    append(Matrix, FlatMatrix),
    flatten(FlatMatrix, FlatList).
	

minimax(Pos, _, Val, DepthLimit):-
DepthLimit =< 0, !,
utility(Pos, Val).

minimax(Pos, BestNextPos, Val, DepthLimit):-
DepthLimit > 0,
bagof(NextPos, move(Pos, NextPos), NextPosList),
NewDepthLimit is DepthLimit - 1,
best(NextPosList, BestNextPos, Val, NewDepthLimit), !.

best([Pos], Pos, Val, DepthLimit):-
minimax(Pos, _, Val, DepthLimit), !.

best([Pos1 | Tail], BestPos, BestVal, DepthLimit) :-
minimax(Pos1, _, Val1, DepthLimit),
best(Tail, Pos2, Val2, DepthLimit),
betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).