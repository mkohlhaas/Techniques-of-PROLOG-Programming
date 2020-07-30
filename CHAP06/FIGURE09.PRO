% FIGURE 6.9: Program for network setting
%----------------------------------------------------
% consult this program and enter the following query:
%   ?- network(Links,Cost).
%----------------------------------------------------
    network(Links,Cost) :-
        setof(X,(Y,D)^distance(X,Y,D),[I|Rest]),
        connect(Rest,[I],Links,0,Cost).

    connect([],_,[],TC,TC) :- !.
    connect(Uncon,Network,[(A,B)|Links],TC,TC1) :-
        setof((D,X,Y,Rest),(select(X,Uncon,Rest),
               member(Y,Network),distance(X,Y,D)),
               [(C,A,B,Remain)|_]),
        TC2 is TC + C,
        connect(Remain,[A|Network],Links,TC2,TC1).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    select(X,[X|T],T).
    select(X,[H|T],[H|R]) :- select(X,T,R).

    distance(X,Y,D) :- dist(X,Y,D).
    distance(X,Y,D) :- dist(Y,X,D).

    dist(se,de,2000).        dist(om,de,780).
    dist(se,om,1400).        dist(om,dc,1000).
    dist(se,sl,750).         dist(om,ho,800).
    dist(se,la,1000).        dist(de,bo,650).
    dist(la,sl,500).         dist(de,dc,450).
    dist(la,om,1100).        dist(de,ho,1000).
    dist(la,ho,1100).        dist(ho,dc,1200).
    dist(sl,om,800).         dist(dc,bo,400).
    dist(sl,ho,1000).
%-----------------------------------------------------



