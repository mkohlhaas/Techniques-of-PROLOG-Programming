% PROBLEM 3.6: The analogy problem
%---------------------------------------------------------------
% consult this program and try the following queries:
%   ?- analogy((1,5),(3,X)).
%   ?- analogy((6,2),(4,X)).
% replace the facts by new facts to describe the second set of
% figures and their relationships and try the following queries:
%   ?- analogy((4,6),(1,X)).
%   ?- analogy((1,3),(4,X)).
%   ?- analogy((5,4),(2,X)).
%---------------------------------------------------------------
    analogy((A,B),(C,X)) :-
        figure(A,FA),figure(B,FB),figure(C,FC),
        relate(FA,FB,Rule),
        relate(FC,FX,Rule),
        figure(X,FX).

    relate(middle(F1,F2),middle(F2,F1),invert).
    relate(middle(F1,F2),left_top(F1,F2),shift_left_top).
    relate(middle(F1,F2),right_top(F1,F2),shift_right_top).

    figure(1,middle(triangle,square)).
    figure(2,middle(circle,triangle)).
    figure(3,middle(square,circle)).
    figure(4,middle(square,square)).
    figure(5,middle(square,triangle)).
    figure(6,middle(triangle,circle)).
    figure(7,middle(circle,square)).
    figure(8,middle(triangle,triangle)).


