% FIGURE 6.8: Collection of test results using setof
%------------------------------------------------------------
% consult this program and enter the following query:
%   ?- test_result(L).
% Now, change the definition of test_result as follows:
%   test_result(L) :-
%      setof(stud_mark(Name,Mark),
%            (student(ID,Name),test(ID,Mark)), L).
% then try the above query again; observe the result.
% Next, add ID^ in front of (student(ID,Name),test(ID,Mark)),
% then try the above query again; observe the result.
%------------------------------------------------------------
    test_result(L) :-
        setof(stud_mark(Name,ID,Mark),
              (student(ID,Name),test(ID,Mark)), L).

    student(78-2123,'Smith J.R.').
    student(84-2201,'Adams P.').
    student(90-3150,'Burns M.I.').
    student(90-5216,'Pollard K.L.').
    student(91-1246,'Mill H.R.').

    test(78-2123,40).
    test(84-2201,65).
    test(90-3150,56).
    test(90-5216,84).
    test(91-1246,92).
%------------------------------------------------------------



