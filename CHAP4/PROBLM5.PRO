% PROBLEM 4.5: Detect straight-line boundaries of an image
%---------------------------------------------------------
% consult this program and enter the following query;
% beware of the brackets around (M,C):
%   ?- detect_line((M,C)).
%---------------------------------------------------------
    detect_line(MC) :-
        threshold(T),
        image(X,Y,G),gradient(X,Y,S), S > T,
        record_transform(X,Y),
        fail.
    detect_line(MC) :-
        find_max(_,0,MC).

    gradient(u,v,_) :- !,fail.
    gradient(X,Y,S) :-
        X1 is X-1, X2 is X+1, Y1 is Y-1, Y2 is Y+1,
        image(X1,Y,G11), image(X2,Y,G12), D1 is G12 - G11,
        image(X,Y1,G21), image(X,Y2,G22), D2 is G22 - G21,
        S is sqrt(D1*D1 + D2*D2),!.

    record_transform(X,Y) :-
        minimum(M1,C1), maximum(M2,C2),
        transform((M1,M2),(C1,C2),(X,Y)),!.

    transform((M,M2),(C1,C2),(X,Y)) :-
        M =< M2, C is Y - M*X,
        (C1 =< C, C =< C2,!,increment((M,C)); true),
        N is M+1,
        transform((N,M2),(C1,C2),(X,Y)),!.
    transform(_,_,_).

    increment(MC) :-
        retract(count(MC,V)),!, V1 is V+1,
        assert(count(MC,V1)).
    increment(MC) :-
        assert(count(MC,1)).

    find_max(_,V,X) :-
        count(X1,V1), V1 > V,!,
        find_max(X1,V1,X).
    find_max(X,_,X).

    threshold(1).
    minimum(-4,0).
    maximum(4,20).

    image(0,0,2).
    image(0,1,2).
    image(0,2,3).
    image(0,3,5).
    image(0,4,2).
    image(1,0,1).
    image(1,1,4).
    image(1,2,4).
    image(1,3,3).
    image(2,0,4).
    image(2,1,5).
    image(2,2,6).
    image(3,0,6).
    image(3,1,7).
    image(4,0,6).
    image(_,_,0).


