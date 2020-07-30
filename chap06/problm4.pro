% PROBLEM 6.4: Detect the palindromes
% -------------------------------------------------------------
% consult this program and try the following queries:
%   ?- palindrome(madam).
%   ?- palindrome('dog a devil deified , deified lived a god').
% -------------------------------------------------------------
palindrome(Word) :-
    name(Word,L),palind(0,L,_).

palind(S,L,P) :-
    change_state(S,L,P,S1,L1,P1),
    palind(S1,L1,P1).
palind(1,[],[]).

change_state(0,[X|L],P,0,L,[X|P]).
change_state(0,L,P,1,L,P).
change_state(0,[_|L],P,1,L,P).
change_state(1,[X|L],[X|P],1,L,P).
