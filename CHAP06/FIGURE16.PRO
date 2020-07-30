% FIGURE 6.16: Prolog programs in modified syntax
%------------------------------------------------------------
% Consult this file and try the following queries:
%   ?- : sue likes What.
%   ?- : snoopy is_atoy.
%   ?- : What is_atoy.
%
% Now try the following query that imitates a Pascal program.
%
%   ?- for I := 1 to 4 do
%          begin
%              X := I*I;
%              writeln(X);
%           end.
%------------------------------------------------------------
:- op(1200,xfx,if).
:- op(1100, fx,:).
:- op(1000,xfy,and).
:- op(500,xf,is_atoy).
:- op(500,xfx,likes).
:- op(500,xfx,plays_with).

% The world of Ann and Sue
%----------------------------------------------------
    sue likes X if ann likes X.
    ann likes X if X is_atoy and ann plays_with X.
    doll is_atoy.
    snoopy is_atoy.
    ann plays_with snoopy.

    : A :- (A if B),(: B).
    : A :- A.
    A and B :- A,B.


% Operators used to imitate Pascal
%---------------------------------------------------
:- op(1150,xfx,do).
:- op(1140,fx,for).
:- op(1140,fx,begin).
:- op(1100,xfx,to).
:- op(600,xfx,':=').


    for I := M to N do G :-  for(I,M,N), G, fail; true.

    begin X;Y :-  X,(begin Y).
    begin end.

    X := Y  :- X is Y.
    writeln(X) :- write(X),nl.

    for(I,I,N) :- I =< N.
    for(I,M,N) :- M < N, M1 is M+1, for(I,M1,N).

%-------------------------------------------------------





