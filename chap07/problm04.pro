% PROBLEM 7.4: The jealous husbands
% ----------------------------------------------------------
% Consult this program and enter the following query to find
% a sequence of moves to bring the husbands and wives safely
% to the mainland without any discontent from the husbands:
%   ?- hill_climb_search(Answer).
% Observe the answer and try to recognize the moves.
% Now, rerun the program by entering the following query:
%   ?- hill_climb_search(Answer),
%      print_answer(Answer),
%      get0(C).
%
% Press any key to terminate.
% ----------------------------------------------------------
initial_state(([],[2,(2,2),(2,2),(2,2),(2,2),(2,2)])).
final_state((_,[_,(0,0),(0,0),(0,0),(0,0),(0,0)])).

next_state(S,S1) :-
    change(S,S1),
    not(jealous(S1)),
    boat_ok(S1).

change((_,[B|Pairs]),([ML,MB,WL,WB],[NB|NewPairs])) :-
    NB is 2-B,
    transfer(B,Pairs,[],[0,0,0,0],[ML,MB,WL,WB],NewPairs).

transfer(_,[],Pairs,C,C,Pairs).
transfer(B,[P|Pairs],MPairs,K,C,NewPairs) :-
    move_pair(B,K,P,K1,P1),
    insert(P1,MPairs,NPairs),
    transfer(B,Pairs,NPairs,K1,C,NewPairs).

move_pair(D,[ML,MB,WL,WB],(M,W),[ML1,MB1,WL1,WB1],(M1,W1)) :-
    move(D,(M,ML,MB),(M1,ML1,MB1)),
    move(D,(W,WL,WB),(W1,WL1,WB1)).

move(D,(D,ML,MB),(1,ML,MB1)) :- MB1 is MB+1.
move(D,(1,ML,MB),(D,ML1,MB)) :- ML1 is ML+1.
move(_,S,S).

insert(X,[],[X]) :- !.
insert(X,[Y|T],[X,Y|T]) :- X @=< Y,!.
insert(X,[Y|T],[Y|R]) :- insert(X,T,R).

jealous((_,[_|Pairs])) :-
    member((M,W),Pairs), M \= W,
    member((W,_),Pairs).

boat_ok((_,[_|Pairs])) :-
    number_onboat(Pairs,N),1 =< N, N =< 3,!;
    final_state((_,[_|Pairs])).

number_onboat([],0).
number_onboat([(M,W)|Pairs],N) :-
    number_onboat(Pairs,K),
    N is K+ (M mod 2)+ (W mod 2).

% ----------------------------------------------------------
% The evaluation function to be used in hill-climbing search
% ----------------------------------------------------------
value((_,[_|Pairs]),Value) :-
    add_up(Pairs,Value).

add_up([],0).
add_up([(M,W)|Pairs],Value) :-
    add_up(Pairs,V),
    Value is V+M+W.

%-----------------------------------------------------------
% Procedure that displays the list of moves
%-----------------------------------------------------------
print_answer([]).
print_answer([(Move,State)|Rest]) :-
    print_move(Move,State),
    print_answer(Rest).

print_move([],_).
print_move([ML,MB,WL,WB],[Boat|Pairs]) :-
    locate(Boat,Location,Direction),write('>'),
    (ML = 0,!; prints([' ',ML,' men land on ',Location])),
    (MB = 0,!; prints([' ',MB,' men get on ',boat])),
    (WL = 0,!; prints([' ',WL,' women land on ',Location])),
    (WB = 0,!; prints([' ',WB,' women get on ',boat])),
    (final_state((_,[_|Pairs])),!;
             prints([', the boat sails to ',Direction]),nl).

locate(0,island,mainland).
locate(2,mainland,island).

prints([]).
prints([X|T]) :- write(X),prints(T).

% ---------------------------------------------------------------
% Program hill-climbing search
% ---------------------------------------------------------------
hill_climb_search(AnsPath) :-
    initial_state(Init),
    hill_climb([Init],AnsPath).

hill_climb([S|_],[S]) :-
    final_state(S),!.
hill_climb([S|Path],[S|AnsPath]) :-
    extend([S|Path],S1),
    hill_climb([S1,S|Path],AnsPath).

extend([S|Path],S1) :-
    best_next_state(S,S1),
    not(member_state(S1,[S|Path])).

best_next_state(S,S1) :-
    setof((V,NS),(next_state(S,NS),value(NS,V)),List),
    member((_,S1),List).

member_state((_,X),[(_,X)|_]).
member_state(X,[_|T]) :- member_state(X,T).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
