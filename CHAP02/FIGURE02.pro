% FIGURE 2.2: Prolog program for the musicians problem
%-----------------------------------------------------
% consult this program and enter the following query:
%   ?- musician_solution(X).
%-----------------------------------------------------
    musician_solution(S) :-
        band_soloists(S),
        first(X,S),plays(X,piano),
        order_mbers(Y,Z,S),
            named(Y,john),plays(Y,sax),
            country(Z,australia),
        order_mbers(Y1,Z1,S),
            named(Y1,mark),country(Y1,us),
            plays(Z1,violin),
        member(U,S),country(U,japan),
        member(V,S),named(V,sam).

    band_soloists(band(soloist(N1,C1,I1),
                       soloist(N2,C2,I2),
                       soloist(N3,C3,I3))).
    named(soloist(N,_,_),N).        % Soloist's name is N
    country(soloist(_,C,_),C).     % His country is C
    plays(soloist(_,_,I),I).       % His instrument is I
    first(X,band(X,_,_)).          % X is first member
    order_mbers(X,Y,band(X,Y,Z)).  % X plays before Y
    order_mbers(X,Z,band(X,Y,Z)).  % X plays before Z
    order_mbers(Y,Z,band(X,Y,Z)).  % Y plays before Z
    member(X,band(X,Y,Z)).         % X is a member
    member(Y,band(X,Y,Z)).         % Y is a member
    member(Z,band(X,Y,Z)).         % Z is a member



