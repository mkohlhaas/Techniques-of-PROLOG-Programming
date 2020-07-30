% PROBLEM 2.2: Solving a murder case
% ---------------------------------------------------
% consult this program and enter the following query:
%   ?- murder_solution(S).
% ---------------------------------------------------
murder_solution(S) :-
    suspects(S),
    motives(M),objects(O),activities(A),
    member((burner,_,cigarette,_),S),
    member((dulles,_,_,walked_pass),S),
    member((_,hate,diary,_),S),
    member((_,_,umbrella,left_house),S),
    member((Person,promotion,_,argued),S),
        man(Person),
    associate(S,M,O,A),
    member((curious,MC,OC,_),S),
        MC \= sacked, OC \= letter,
        OC \= pistol, MC \= hate,
    member((_,sacked,OS,_),S),
        OS \= letter,
    member((ablaze,harassed,OA,AA),S),
        OA \= letter, AA \= commit_murder.

associate([],[],[],[]).
associate([(_,M,O,A)|Rest],Ms,Os,As) :-
    select(M,Ms,RMs),
    select(O,Os,ROs),
    select(A,As,RAs),
    associate(Rest,RMs,ROs,RAs).

select(X,[X|R],R).
select(X,[Y|R],[Y|S]) :-
    select(X,R,S).

member(X,[X|_]).
member(X,[_|R]) :- member(X,R).

man(Name) :- member(Name,[burner,evilson]).

suspects([(ablaze,_,_,_),(burner,_,_,_),
          (curious,_,_,_),(dulles,_,_,_),
          (evilson,_,_,_)]).

motives([harassed,abandoned,sacked,promotion,hate]).
objects([pistol,umbrella,cigarette,diary,letter]).
activities([commit_murder,argued,left_house,
            rang_victim,walked_pass]).
