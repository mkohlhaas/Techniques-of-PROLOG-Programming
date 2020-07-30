% PROBLEM 6.5: Job assignment
% ---------------------------------------------------------
% Consult this program and enter the following query to
% find an assignment of jobs to the given machines with
% minimum total cost:
%   ?- assign_jobs(JobList,Cost).
%
% Now, replace the current cost table with a new one (with
% 8 jobs) by using the following command:
%   ?- abolish(table/3),consult('\chap6\jobs1.dat').
% Then re-enter the above query to find the optimal job
% assignment with the new cost table.
%
% Finally, to run the program with larger number of jobs,
% replace the goal
%     setof((K,L),zero_lines(N,T,[],0,L,K),[(ML,Lines)|_]),
% in the procedure 'adjust_cost' with the new goal
%     setof_lines(N,T,Lines,ML),
% and enter the following command to load a new cost table:
%   ?- abolish(table/3),consult('\chap6\jobs2.dat').
% Then re-enter the above query to find an optimal job
% assignment for ten machines. Beware, this run may take
% up to one minute, as it saves space but does more work.
%
% ----------------------------------------------------------
:- op(500,xfy,:).

assign_jobs(JobList,MinCost) :-
    cost_table(N,T),
    reduce_cost(N,T,Table,Cost),
    optimize(N,Table,Cost,JobList,MinCost).

cost_table(N,Table) :-
    make_table(Table),
    length(Table,N).

reduce_cost(N,T,Table,Cost) :-
    reduce_rows(T,T1,0,V),transpose(N,T1,T2),
    reduce_rows(T2,T3,V,Cost),transpose(N,T3,Table).

reduce_rows([R|Rs],[R1|Rs1],V,V1) :-
    (member(e(_,_,0),R),!, R = R1, V = V2;
     setof(C,(I,J)^member(e(I,J,C),R),[C0|_]),
     setof(e(I,J,C1),
       C^ (member(e(I,J,C),R),C1 is C - C0), R1),
     V2 is V + C0),
    reduce_rows(Rs,Rs1,V2,V1).
reduce_rows([],[],V,V).

optimize(_,T,Cost,JobList,Cost) :-
    solution(T,[],JobList),!.
optimize(N,T,Cost,JobList,MinCost) :-
    adjust_cost(N,T,T1,ACost,M),
    NCost is Cost + ACost*(N-M),
    optimize(N,T1,NCost,JobList,MinCost).

solution([],_,[]) :- !.
solution([R|Rs],List,[(I,J)|IJList]) :-
    member(e(I,J,0),R),not(member(J,List)),
    solution(Rs,[J|List],IJList).

adjust_cost(N,T,T1,C1,ML) :-
    setof((K,L),zero_lines(N,T,[],0,L,K),[(ML,Lines)|_]),
    setof(C,(I,J)^uncrossed(Lines,T,I,J,C),[C1|_]),
    adjust_rows(Lines,C1,T,T1).

zero_lines(N,T,L,K,L1,K1) :-
    uncrossed(L,T,I,J,0),!,K < N, H is K+1,
    (Line = row:I; Line = col:J),
    zero_lines(N,T,[Line|L],H,L1,K1).
zero_lines(_,_,L,K,L,K).

uncrossed(L,T,I,J,C) :-
    member([e(I,K,D)|R],T), not(member(row:I,L)),
    member(e(I,J,C),[e(I,K,D)|R]), not(member(col:J,L)).

adjust_rows(Lines,C0,[R|Rs],[R1|Rs1]) :-
    setof(e(I,J,C1),
    C^ (member(e(I,J,C),R),adjust(Lines,I,J,C,C0,C1)),R1),
    adjust_rows(Lines,C0,Rs,Rs1).
adjust_rows(_,_,[],[]).

adjust(L,I,J,C,C0,C1) :-
    member(row:I,L),member(col:J,L),!, C1 is C + C0;
    (member(row:I,L);member(col:J,L)),!, C1 is C;
    C1 is C - C0.

make_table(Table) :-
    setof(Row,
          H^setof(e(I,J,X),(table(H,J,X),I = H),Row),
          Table).

transpose(N,Table,Table1) :-
    setof(Col,(M,X,Row)^
              (nth_member(M,X,Row),
               (M > N,!,fail;
                setof(X,Row^member(Row,Table),Col))),
         Table1).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

nth_member(1,X,[X|_]).
nth_member(N,X,[_|T]) :- nth_member(M,X,T), N is M+1.

% length([],0).
% length([H|T],N) :-
%     length(T,M), N is M+1.

table(1,1,6).
table(3,1,8).
table(1,2,5).
table(3,2,6).
table(1,3,3).
table(3,3,8).
table(1,4,6).
table(3,4,9).
table(2,1,5).
table(4,1,3).
table(2,2,6).
table(4,2,6).
table(2,3,8).
table(4,3,5).
table(2,4,12).
table(4,4,8).

% ------------------------------------------------------------------
% A space-efficient procedure to find the minimum set of zero-lines 
% ------------------------------------------------------------------
setof_lines(N,T,_,_) :- assert(lines(_,100)),
    zero_lines(N,T,[],0,L,K),lines(L0,K0), K < K0,
    retract(lines(L0,K0)),assert(lines(L,K)),fail.
setof_lines(_,_,Lines,ML) :- retract(lines(Lines,ML)).
