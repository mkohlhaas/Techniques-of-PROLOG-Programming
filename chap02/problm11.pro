% PROBLEM 2.11: A declarative program for jobs assignment
% -------------------------------------------------------
% consult this program and enter the following query:
%   ?- assign_jobs(4,JobList,Cost).
% -------------------------------------------------------
assign_jobs(N,JobList,Cost) :-
    list(1,N,Macs),
    assignment(Macs,JobList,Cost),
    minimum_cost(Macs,Cost).

assignment(Macs,JobList,Cost) :-
    permutation(Macs,Jobs),
    pairs(Macs,Jobs,JobList,Cost).

minimum_cost(Macs,Cost) :-
    not((assignment(Macs,_,Cost2),Cost2 < Cost)).

pairs([],[],[],0).
pairs([M|Ms],[J|Js],[(M,J)|L],Cost) :-
    pairs(Ms,Js,L,C),table(M,J,C1),
    Cost is C + C1.

permutation([],[]).
permutation(L,[H|T]) :-
    select(H,L,R),permutation(R,T).

select(H,[H|T],T).
select(X,[H|T],[H|T1]) :- select(X,T,T1).

list(I,N,[]) :- I > N.
list(I,N,[I|L]) :-
    I =< N, I1 is I+1,list(I1,N,L).

table(1,1,6).            table(3,1,8).
table(1,2,5).            table(3,2,6).
table(1,3,3).            table(3,3,8).
table(1,4,6).            table(3,4,9).
table(2,1,5).            table(4,1,3).
table(2,2,6).            table(4,2,6).
table(2,3,8).            table(4,3,5).
table(2,4,12).           table(4,4,8).
