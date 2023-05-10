startGame:-
write("Do you want x or o?"), nl,
read(Human),
% Human player can be x or o but x is always MAX and o is MIN
StartState = ['-','-','-','-','-','-','-','-','-'],
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
nth0(HumanMove, State, '-', TmpList),
nth0(HumanMove, NextState, Human, TmpList),
otherPlayer(Human, Computer),
play([Computer, NextState], Human).

play([Computer, State], Human):-
nl, draw(State), nl,
computerTurn([Computer,State], NextState),
play(NextState, Human).

computerTurn(State, Next):-
minimax(State, Next, _).

isTerminal(State):-
getWinner(State, Winner), write(Winner), write(' wins!'), nl, !.

isTerminal(State):-
not(member('-', State)), write('It\'s a draw!'), nl.

getWinner(State, Winner):-
( (State = [Z,Z,Z,_,_,_,_,_,_], !);
(State = [_,_,_,Z,Z,Z,_,_,_], !);
(State = [_,_,_,_,_,_,Z,Z,Z], !);
(State = [Z,_,_,Z,_,_,Z,_,_], !);
(State = [_,Z,_,_,Z,_,_,Z,_], !);
(State = [_,_,Z,_,_,Z,_,_,Z], !);
(State = [Z,_,_,_,Z,_,_,_,Z], !);
(State = [_,_,Z,_,Z,_,Z,_,_], !) ), Z \= '-', Winner = Z.

draw([]):-!.

draw([H|T]):-
length(T, N),
write(H),
(0 is N mod 3 -> nl ; write(' ')),
draw(T).

minimax(Pos, BestNextPos, Val):-
bagof(NextPos, move(Pos, NextPos), NextPosList),
best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val):-
utility(Pos, Val).

best([Pos], Pos, Val):-
minimax(Pos, _, Val), !.

best([Pos1 | Tail], BestPos, BestVal) :-
minimax(Pos1, _, Val1),
best(Tail, Pos2, Val2),
betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
isMinPlayer(Pos1), % next (Pos1) is MIN, so the parent is MAX
(Val1 >= Val2 -> (BestPos = Pos1, BestVal is Val1, !); 
(BestPos = Pos2, BestVal is Val2)), !.

betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal) :-
(
Val1 =< Val2 -> (BestPos = Pos1, BestVal is Val1, !); 
(BestPos = Pos2, BestVal is Val2)
), !.

move([Player, State], Next):-
not(getWinner(State,_)),
getMove([Player, State], Next).

getMove([Player, State], [NextPlayer, NextState]):-
otherPlayer(Player, NextPlayer),
State = ['-'|T],
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