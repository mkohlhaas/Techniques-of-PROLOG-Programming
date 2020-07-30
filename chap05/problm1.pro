% PROBLEM 5.1: The stable marriage problem
% -----------------------------------------------------------------
% consult this program and invoke the system by entering the query:
%   ?- marriage_solver.
% then enter the number 1. to choose the first marrying group.
% Try again with number 2. to solve the second marrying problem.
% -----------------------------------------------------------------
:- op(500,xfy,:).

marriage_solver :-
    read_information(Men,Women),
    solve_marriage(Men,Women,Sol),
    print_solution(_,Sol),
    test_for_termination.
marriage_solver :-
    finalize.

read_information(Men,Women) :- nl,
    write('Enter marrying group number, '),
    write('terminated with a period: '),
    read(N),
    marriage_group(N,Men,Women).

solve_marriage(Men,Women,Sol) :-
    form_couples(Men,Women,[],Sol).

form_couples([],[],Sol,Sol).
form_couples([M|Ms],WL,PairList,Sol) :-
    select(W,WL,WL1),
    not(unstable((M,W),PairList)),
    form_couples(Ms,WL1,[(M,W)|PairList],Sol).

unstable(Pair,PairList) :-
    member(Pair1,PairList),
    cross_prefer(Pair,Pair1).

cross_prefer((M,W),(M1,W1)) :-
    prefer(M,W1,W),prefer(W1,M,M1);
    prefer(W,M1,M),prefer(M1,W,W1).

prefer(_:L,P1:_,P2:_) :-
    append(_,[P1|Rest],L),
    member(P2,Rest).

print_solution(0,[]) :- !,nl.
print_solution(K1,[(M:_,W:_)|Rest]) :-
    print_solution(K,Rest),
    K1 is K+1,
    nl,write('Couple '),write(K1:M+W).

test_for_termination :- nl,nl,
    write('Press any key for more solution,<ESC> to stop'),
    get0(27).

finalize :- nl,
    write('No more solutions!'),nl,
    write('Another marriage problem? <y/n> '),
    get0(C), (C = 121,!, marriage_solver; true).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[H|T],[H|R]) :- select(X,T,R).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

marriage_group(1,
    [john:[annie,suzie,wendy],
     mark:[wendy,annie,suzie],
     tony:[wendy,suzie,annie]],
    [annie:[tony,john,mark],
     suzie:[mark,john,tony],
     wendy:[john,mark,tony]]).

marriage_group(2,
    [allan:[verra,xania,zonie,wendy,yamie],
     bobby:[zonie,xania,wendy,verra,yamie],
     chris:[verra,wendy,xania,yamie,zonie],
     danny:[yamie,xania,zonie,wendy,verra],
     ersky:[yamie,xania,verra,wendy,zonie]],
    [xania:[allan,bobby,danny,ersky,chris],
     yamie:[ersky,danny,chris,allan,bobby],
     zonie:[danny,bobby,allan,chris,ersky],
     verra:[bobby,allan,ersky,chris,danny],
     wendy:[chris,bobby,allan,danny,ersky]]).
