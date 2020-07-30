% PROBLEM 4.4: Detect a feature in an image
% ---------------------------------------------------
% consult this program and enter the following query:
%   ?- detect_feature(Location).
% ---------------------------------------------------
detect_feature(XY) :-
    image_size(IS),
    template_size(TS),
    best_template_match(IS,TS,XY).

best_template_match(IS,TS,_) :-
    location(IS,TS,XY),
    cross_correlation(XY),
    fail.
best_template_match(_,_,XY) :-
    find_max(_,0,XY).

location((IX,IY),(TX,TY),(X,Y)) :-
    image(X,Y,_),
    X + TX =< IX,
    Y + TY =< IY.

cross_correlation(XY) :-
    assert(cross_value(XY,0)),
    template(I,J,V),
    calculate(XY,(I,J),V),
    fail.
cross_correlation(_).

calculate((X,Y),(I,J),TV) :-
    H is X + I, K is Y + J,
    image(H,K,IV),
    retract(cross_value((X,Y),V)),
    V1 is V + IV*TV,
    assert(cross_value((X,Y),V1)),!.

find_max(_,V,X) :-
    cross_value(X1,V1), V1 > V,!,
    find_max(X1,V1,X).
find_max(X,_,X).

image_size((6,4)).
template_size((5,2)).

template(0,0,0).
template(0,1,0).
template(0,2,3).
template(1,0,0).
template(1,1,2).
template(1,2,3).
template(2,0,1).
template(2,1,0).
template(2,2,3).
template(3,0,1).
template(3,1,0).
template(3,2,3).
template(4,0,1).
template(4,1,2).
template(4,2,0).
template(5,0,1).
template(5,1,0).
template(5,2,0).

image(0,0,0).
image(0,1,0).
image(0,2,0).
image(0,3,0).
image(0,4,0).
image(1,0,1).
image(1,1,0).
image(1,2,1).
image(1,3,3).
image(1,4,1).
image(2,0,0).
image(2,1,0).
image(2,2,2).
image(2,3,3).
image(2,4,1).
image(3,0,1).
image(3,1,0).
image(3,2,0).
image(3,3,2).
image(3,4,0).
image(4,0,1).
image(4,1,1).
image(4,2,1).
image(4,3,3).
image(4,4,0).
image(5,0,0).
image(5,1,1).
image(5,2,2).
image(5,3,3).
image(5,4,0).
image(6,0,0).
image(6,1,1).
image(6,2,0).
image(6,3,0).
image(6,4,1).
