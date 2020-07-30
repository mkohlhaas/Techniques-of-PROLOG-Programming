% PROBLEM 2.6: Travelling between cities
% ---------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers:
%   ?- travelling(albury,sydney).
%   ?- travelling(yass,wollongong).
%   ?- travelling(melbourne,brisbane).
%   ?- travelling(melbourne,cairns).
%   ?- travelling(queanbeyan,City),
%   ?- travelling(City,goulburn).
%   ?- travelling(City1,City2).
% Modify the program to produce the route between two
% cities if travelling is possible.
% ---------------------------------------------------
travelling(A,B) :- travel(A,B,[A]).

travel(A,B,_) :- direct_travel(A,B).
travel(A,B,List) :-
    direct_travel(A,C),
    non_visited(C,List),
    travel(C,B,[C|List]).

non_visited(_,[]).
non_visited(C,[C1|List]) :-
    C \= C1,
    non_visited(C,List).

direct_travel(A,B) :-
    direct_way(A,B); direct_way(B,A).

direct_way(canberra,goulburn).
direct_way(canberra,queanbeyan).
direct_way(canberra,yass).
direct_way(yass,albury).
direct_way(albury,melbourne).
direct_way(goulburn,sydney).
direct_way(goulburn,wollongong).
direct_way(sydney,newcastle).
direct_way(sydney,newengland).
direct_way(newcastle,goldcoast).
direct_way(goldcoast,brisbane).
direct_way(darwin,cairns).
direct_way(darwin,goldbury).
direct_way(goldbury,perth).
direct_way(cairns,pointcook).
direct_way(cairns,alicespring).
