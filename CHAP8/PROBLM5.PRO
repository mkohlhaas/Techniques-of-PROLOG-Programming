% PROBLEM 8.5: A simple technique of obtaining the proof of a goal
%-------------------------------------------------------------------
% Consider the program given at the end of this file. Consult
% this file and enter the following query to obtain the proof of
% the goal p(X):
%   ?- trans_program, p(X,Proof).
%-------------------------------------------------------------------
    transform((A :- B),(A1 :- B1)) :- !,
        trans_body(B,PA,B1),
        add_argument(A,PA,A1).
    transform(A,A1) :-
        add_argument(A,true,A1).

    trans_body((B,Bs),((B :- PB),PBs),(B1,B1s)) :- !,
        add_argument(B,PB,B1),
        trans_body(Bs,PBs,B1s).
    trans_body(B,(B :- PB),B1) :-
        add_argument(B,PB,B1).

    add_argument(A,X,A1) :-
        A =.. [P|Xs], append(Xs,[X],Ys),
        A1 =.. [P|Ys].

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

    trans_program :-
        rule(R),
        transform(R,R1),
        assert(R1),
        fail.
    trans_program.

% The sample program for proof collection
%-----------------------------------------------------------------
    rule((p(X) :- a(X),b(X))).
    rule((a(X) :- c(X),d(X))).
    rule((b(X) :- e(X))).
    rule(c(1)).
    rule(c(2)).
    rule(d(X)).
    rule(e(1)).

%------------------------------------------------------------------


