% PROBLEM 6.2: Define the predicate find_all
% -------------------------------------------------------
% Consult this program and try the following query.
% Note: Some Prolog systems may allow a goal "retract"
% to be backtracked on a newly created fact. To prevent
% this happen, the goal "retract(answer(L1))" in this
% program is enclosed whithin an "once" predicate.
%
%   ?- find_all(stud_mark(Name,Mark),
%              (student(ID,Name),test(ID,Mark)), List).
% -------------------------------------------------------
find_all(X,G,_) :-
    assert(answer([])),
    G,
    my_once(retract(answer(L1))),
    assert(answer([X|L1])),
    fail.
find_all(_,_,L) :-
    retract(answer(L)).

student(78-2123,'Smith J.R.').
student(84-2201,'Adams P.').
student(90-3150,'Burns M.I.').

test(78-2123,40).
test(90-3150,56).
test(84-2201,65).

my_once(P) :- call(P),!.
