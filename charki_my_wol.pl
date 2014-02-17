%writing to screen so don't need to return. 


:- use_module(library(lists)).
%:- use_module(library(ordsets)).

test_strategy(N, FPS, SPS) :-
		datime(Initial),
		test_help(N,FPS,SPS,0,0,0,0,250,0,Draws, Bwin, Rwin,Total, Small,Long),
		datime(Final),
		A is Total / N,		
		write(A), nl,
		write(Draws), nl,
		write(Bwin), nl,
		write(Rwin), nl,
		write(Small), nl,
		write(Long), nl,
		Gt is (Final-Initial) / N,
		write(Gt). 
		


test_help(0, _,_, Draws, Bwin, Rwin, AvgGL,S,L, Draws, Bwin, Rwin,AvgGL, S, L).

test_help(N, FPS, SPS, D, Bw, Rw, Agl,S,L, Draws, Bwin, Rwin, AvgGL, Small,Long) :-
 
	play(quiet, FPS, SPS, TotalMoves, Winner),
	Dec is N-1, 
	Na is Agl + TotalMoves,
	%write(Winner),
	winner(Winner,D,Bw,Rw,Nd,Nb,Nr),
	
	
	small(S,TotalMoves, Ns),
	biggest(L,TotalMoves, Nl),	

	test_help(Dec, FPS, SPS, Nd, Nr, Nb, Na, 
			Ns, Nl, Draws, Bwin, Rwin, AvgGL, Small, Long).

winner(draw,D,B,R,Nd,B,R) :- Nd is D+1.
winner(b,D,B,R,D,Nb,R) :- Nb is B+1.
winner(r,D,B,R,D,B,Nr) :- Nr is R+1.
winner(stalemate,D,B,R,Nd,B,R) :- Nd is D+1.

small(S,T,T) :- T < S. 
small(S,T,S) :- T >= S.		
	
biggest(L,T,L) :- L >= T, L<250. 
biggest(L,T,T) :- T > L.

big(L, L) :- L < 250.


test_strat(N, FPS, SPS) :- 
	make(N, List1, List2, FPS, SPS),
	findall(K==r, member(K,List1), Reds),
	findall(Z == draw, member(Z,List1), Draws),
	findall(Y==stalemate,member(Y,List1), Sm),
	findall(X==b, member(X,List1), Blues),
	longest(List2, L),
	shortest(List2, S),
	list_sum(List2, Total), 
	Avg is Total/ N,
	length(Draws, D),
	length(Sm, M),
	Dr is D + M,
	length(Blues, B),
	length(Reds, R), 
	write(Avg), nl,
	write(Dr), nl,
	write(B), nl,
	write(R), nl,
	write(S), nl,
	write(L). 
		 
	


make(0, [], [], _, _).
make(N, [Winner|L1], [TotalMoves|L2], FPS, SPS) :- 
	play(quiet, FPS, SPS, TotalMoves, Winner),
	Dec is  N-1,
	make(Dec, L1, L2, FPS, SPS).

longest(L, T) :-
   member(T, L),
   T < 250,
   \+ (member(TotalMove,L), TotalMove > T).

shortest(L, T) :-
   member(T, L),
   \+ (member(TotalMove,L), TotalMove < T).

smallest(L, Mred, Move) :-
   member([K, Mred, Move], L),
   length(K, N),
   \+ ((member([O,_,_],L), length(O, P)), P < N). 

large(L, Mred, Move) :- 
   member([K, Mred, Move], L),
   length(K, N),
   \+ ((member([O,_,_],L), length(O, P)), P > N). 


list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([Item1+Item2|Tail], Total).


bloodlust('r',[AliveBlues, AliveReds],[AliveBlues, MR], Move) :-
	%Gets the list of moves, A,B is current and MA, MB place to move to
	generate_moves(AliveReds, AliveBlues, PossMoves),
	findall([NewBlue, Mred, Mov], 
		alter_generation('r', PossMoves,
		[AliveBlues, AliveReds],[NewBlue, _ ], Mred, Mov) , Blues),
	smallest(Blues, MR, Move).

bloodlust('b',[AliveBlues, AliveReds],[MB, AliveReds], Move) :-
	generate_moves(AliveBlues, AliveReds, PossMoves),
	findall([NewReds, MBlue, Mov], 
		alter_generation('b', PossMoves,
		[AliveBlues, AliveReds],[_ , NewReds], MBlue, Mov) , Reds),
	smallest(Reds, MB, Move).

alter_generation('r',PossMoves, [AliveBlues, AliveReds],[NewBlue, NewAliveReds], Mred, Move) :-
	member(Move, PossMoves),
	alter_board(Move, AliveReds, Mred),
	next_generation([AliveBlues, Mred], [NewBlue, NewAliveReds]).

alter_generation('b',PossMoves, [AliveBlues, AliveReds],[NewBlue, NewAliveReds], MBlue, Move) :-
	member(Move, PossMoves),
	alter_board(Move, AliveBlues, MBlue),
	next_generation([MBlue, AliveReds], [NewBlue, NewAliveReds]).



generate_moves(AliveReds, AliveBlues, PossMoves) :- 
	findall([A,B,MA,MB] ,(member([A,B], AliveReds),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB], AliveReds),
	              \+member([MA,MB], AliveBlues)),
		      PossMoves).

self_preservation('r',[AliveBlues, AliveReds],[AliveBlues, MR], Move) :- 
	generate_moves(AliveReds,AliveBlues, PossMoves),
	findall([NewReds, Mred, Mov], 
		alter_generation('r', PossMoves,
		[AliveBlues, AliveReds],[_, NewReds ], Mred, Mov) , Reds),
	large(Reds,MR, Move).
		
self_preservation('b',[AliveBlues, AliveReds],[ MB, AliveReds], Move) :- 
	generate_moves(AliveBlues,AliveReds, PossMoves),
	findall([NewBlues, Mblue, Mov], 
		alter_generation('b', PossMoves,
		[AliveBlues, AliveReds],[ NewBlues, _ ], Mblue, Mov) , Blues),
	large(Blues, MB, Move).	

land_grab('r',[AliveBlues, AliveReds],[AliveBlues, MR], Move) :- 
	generate_moves(AliveReds,AliveBlues, PossMoves),
	findall([NewBlues, NewReds, Mred, Mov], 
		alter_generation('r', PossMoves,
		[AliveBlues, AliveReds],[ NewBlues, NewReds ], Mred, Mov) , Diffr),
	diff('r', Diffr, MR, Move).

land_grab('b',[AliveBlues, AliveReds],[MB, AliveReds], Move) :- 
	generate_moves(AliveBlues,AliveReds, PossMoves),
	findall([NewBlues, NewReds, MBlue, Mov], 
		alter_generation('b', PossMoves,
		[AliveBlues, AliveReds],[ NewBlues, NewReds ], MBlue, Mov) , Diffr),
	diff('b', Diffr, MB, Move).

diff('r', Diffr, Mred, Move) :-
	differ('r', Diffr, Mred, Move, Diff),
	write(Diff), nl, nl,
	\+ (differ('r', Diffr, _, _, Other) , Other > Diff).

diff('b', Diffr, Mblue, Move) :-
	differ('b', Diffr, Mblue, Move, Diff),
	\+ (differ('b', Diffr, _, _, Other), Other > Diff).

differ('b', Diffr, Mblue, Move, Diff) :- 
	member([B, R, Mblue, Move], Diffr),
	length(B, BN),
	length(R, RN),
	Diff is BN - RN.

differ('r', Diffr, Mred, Move, Diff) :- 
	member([B, R, Mred, Move], Diffr),
	length(B, BN),
	length(R, RN),
	Diff is RN - BN.

minimax('r',[AliveBlues, AliveReds],[AliveBlues, MR], Move) :- 
	generate_moves(AliveReds,AliveBlues, PossMoves),
	%generates all the next gen boards 1st gen, Produces all the available moves
	findall([NewBlues, NewReds, Mred, Mov], 
	alter_generation('r', PossMoves,
		[AliveBlues, AliveReds],[ NewBlues, NewReds ], Mred, Mov), D),
	
	findall([Difference, MoveRed, Mo], minimise(D, [Difference, MoveRed, Mo]), MIN),
	maximise(MIN, MR, Move).
	
	
	
	
eval('b', Diffr, Mblue, Move, Diff) :-
	differ('b', Diffr, Mblue, Move, Diff),
	\+ (differ('b', Diffr, _, _, Other) , Other > Diff).

eval('r', Diffr, Mred, Move, Diff) :-
	differ('r', Diffr, Mred, Move, Diff),
	\+ (differ('r', Diffr, _, _, Other) , Other > Diff).



%This will return me all the minimums of the opponents, And the red move which caused it.
minimise(D, [Diff, Mr, Mo]) :-
	%generates all the opponents move from that 1st gen
	member([NB, NR, Mr, Mo], D),
	generate_moves(NB, NR, BluePoss), 
	%creates the second generation by opponent blue moving. 2nd gen.
	findall([NewNewBlues, NewNewReds, MBlue, M], 
		alter_generation('b', BluePoss,
		[NB, NR],[NewNewBlues, NewNewReds ], MBlue, M) , Diffr),
	%finds the utility of the blues move in reaction to red move.
	%Tries land_grab to minimise our land_grab.
	%This eval only gets blues moves for 1 of the red moves!
	eval('b', Diffr, _, _, Diff).

%Gets the move that will maximmise our utility, knowing the the opponent will try to minimise it
maximise(List, Mred, Mo) :-
	member([Diff, Mred, Mo], List),
	\+ (member([Di, _, _], List), Di < Diff).
	
	




	
	
	

	













