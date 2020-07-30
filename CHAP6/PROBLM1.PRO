% PROBLEM 6.1: A database on courses and students
%-------------------------------------------------------
% consult this program and try the following queries:
%   ?- check_clash([ics,dma,co1,is1],L).
%   ?- check_clash([dma,is1,se1,is2],L).
%   ?- check_prereq(78-2123,[is1,is2,co1,ps2],L).
%   ?- check_prereq(84-2201,[ma2,is2,ps2,se1],L).
%-------------------------------------------------------
:- op(500,xfy,:).

    check_clash(R,L) :-
        setof((U,V),(member(U,R),member(V,R),
                     U @< V, clash(U,V)),L),!.
    check_clash(_,[]).

    clash(U,V) :-
        unit(U,_,_,TU),unit(V,_,_,TV),
        member(T,TU),member(T,TV).

    check_prereq(ID,R,L) :-
        student(ID,_,_,Credits,_),
        setof(U,(member(U,R),
                 not(meet_prereq(U,Credits))),L),!.
    check_prereq(_,_,[]).

    meet_prereq(Unit,Credits) :-
        unit(Unit,_,Prerequisites,_),
        satisfy(Prerequisites,Credits).

    satisfy([],_) :- !.
    satisfy([U|Us],Credits) :-
        member((U,G),Credits),pass(G),
        satisfy(Us,Credits).

    pass(G) :- member(G,['P','CR','DI','HD']).

    member(X,[X|T]).
    member(X,[H|T]) :- member(X,T).

% UNIT(UNITCODE,CREDITPT,PREREQUISITES,TIME-TABLE)
unit(ics,3,[],[(mon,9:30),(tue,9:30),(thu,11:30)]).
unit(dma,3,[],[(tue,8:30),(wed,8:30),(fri,10:30)]).
unit(dsa,3,[],[(mon,10:30),(wed,14:30),(fri,14:30)]).
unit(is1,3,[ics],[(tue,9:30),(wed,9:30),(thu,9:30)]).
unit(co1,3,[dsa],[(mon,9:30),(tue,9:30),(thu,9:30)]).
unit(se1,3,[dma,dsa,is1],[(mon,14:30),(wed,16:30),(fri,16:30)]).
unit(ps2,3,[dma,dsa,is1],[(mon,14:30),(wed,14:30),(fri,14:30)]).
unit(is2,3,[is1],[(tue,8:30),(thu,9:30),(fri,10:30)]).
unit(ma1,3,[],[(mon,9:30),(wed,9:30),(fri,9:30)]).
unit(ma2,3,[ma1],[(mon,4:30),(tue,10:30),(thu,4:30)]).

% STUDENT(ID,NAME,COURSE,PAST-CREDITS,CURR-PROGRAM)
student(78-2123,'Smith J.R.','Computer Science',
       [(ics,'P'),(dma,'P'),(dsa,'CR'),(is1,'NX')],
       [ps2,co1,is1,se1]).
student(84-2201,'Adams P.','Computer Engineering',
       [(ics,'CR'),(dma,'P'),(dsa,'DI'),(ma1,'DI')],
       [ma2,co1,is1,se1]).
student(86-7623,'Burns F.M.','Computer Science',
       [(ics,'NX'),(dma,'NX'),(ma1,'P')],
       [ma2,ics,dma]).
student(88-5540,'Collins Q.','Computer Science',
       [(ics,'P'),(dma,'P'),(ac1,'P'),('sta','P')],
       [dsa,co1,is1,ac2]).
%----------------------------------------------------


