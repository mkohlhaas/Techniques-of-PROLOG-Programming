% FIGURE 2.2: Prolog program for the musicians problem
% -------------------------------------------------------------------------------------------------
% consult this program and enter the following query:
%   ?- musician_solution(S).
%   S = band(soloist(mark, us, piano), soloist(john, japan, sax), soloist(sam, australia, violin))
% -------------------------------------------------------------------------------------------------
musician_solution(S) :-
    first(X,S),plays(X,piano),
    plays_before(A,B,S),
        named(A,john),plays(A,sax),
        country(B,australia),
    plays_before(A1,B1,S),
        named(A1,mark),country(A1,us),
        plays(B1,violin),
    member(U,S),country(U,japan),
    member(V,S),named(V,sam).

named(soloist(N,_,_),N).       	% Soloist's name is N
country(soloist(_,C,_),C).     	% His country is C
plays(soloist(_,_,I),I).       	% His instrument is I
first(X,band(X,_,_)).          	% X is first member
plays_before(X,Y,band(X,Y,_)). 	% X plays before Y
plays_before(X,Z,band(X,_,Z)). 	% X plays before Z
plays_before(Y,Z,band(_,Y,Z)). 	% Y plays before Z
member(X,band(X,_,_)).         	% X is a member
member(Y,band(_,Y,_)).         	% Y is a member
member(Z,band(_,_,Z)).         	% Z is a member
