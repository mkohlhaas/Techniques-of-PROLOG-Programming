% FIGURE 5.14: Menu-handling in Prolog
% ----------------------------------------------------------
% Before consulting this program, check your Prolog system
% for the built-in predicate equivalent to the predicate cls
% (clear-screen) used in this program. Replace every goal cls
% in this program with its equivalent (for an example of such
% replacement, see file \chap5\figure12.pro) or remove them
% all together if there is no such built-in predicate in your
% Prolog system.
%
% Consult this program and try the following query:
%    ?- display_menu(dbadmin).
% then enter the number 3. to move to the next menu,
% then enter the number 0. to return to the main menu.
% Now write the procedures process(dbadmin,I), I = 1,2,
% and repeat the above query with selection number 1 or 2.
% Next write the procedures process(createdb,I), I = 1,2,3,
% and repeat the above process with selection numbers 1,2,3.
% ----------------------------------------------------------
display_menu(MenuName):-
    put_menu(MenuName,Response),
    accept(Request,Response),
    process(MenuName,Request).

accept(Request,[Prompt|Range]) :-
    repeat,
        nl,nl,
        write(Prompt),
        read(Request),
    member(Request,Range).

put_menu(dbadmin,['    Select service required: ',0,1,2,3]) :-
    cls,
    nl,write('    DB10 DATABASE ADMINISTRATOR'),
    nl,write('    ----------------------------------------'),
    write('----------'),
    nl,write('    We provide the following services :'),
    nl,nl,write('             1:  Search existing databases'),
       nl,write('             2:  Update existing databases'),
       nl,write('             3:  Create new databases'),
       nl,write('             0:  Exit').

put_menu(createdb,['    Select data structure required: ',0,1,2,3]) :-
    cls,
    nl,write('    CREATE NEW DATABASE'),
    nl,write('    ----------------------------------------'),
    write('----------'),
    nl,write('    Enter database name: '),
    read(DBName),assert(database(DBName)),
    nl,write('    We support the following data structures:'),
    nl,nl,write('             1:  Sequential data'),
       nl,write('             2:  AVL-tree'),
       nl,write('             3:  B-tree'),
       nl,write('             0:  Return to main menu').

process(dbadmin,0).
process(dbadmin,1)  :- exercise(1).
process(dbadmin,2)  :- exercise(2).
process(dbadmin,3)  :- display_menu(createdb).
process(createdb,0) :- display_menu(dbadmin).
process(createdb,1) :- exercise(3).
process(createdb,2) :- exercise(4).
process(createdb,3) :- exercise(5).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

exercise(_) :- 
    write('    You are asked to design this menu, and try again.'),
    fail.
