% PROBLEM 10.1: Parsing a French verse
%------------------------------------------------------------------
% This file contains a definite clause grammar to parse some French
% verses. Consult this file and try the following queries:
%    ?- sentence([le,trottoir,de,la,rue,est,sonore,a,mon,pas],[]).
%    ?- sentence([les,jardins,etroits,sont,fleuris,de,lilas],[]).
%    ?- sentence([la,rue,etroite,est,fleurie,comme,un,jardin],[]).
%    ?- sentence([mon,pas,est,sonore,sur,le,trottoir,de,lilas],[]).
%------------------------------------------------------------------
    sentence --> noun_phrase(P,G),verb_phrase(P,G).

    noun_phrase(P,G) --> noun_part(P,G).
    noun_phrase(P,G) --> noun_part(P,G),qualify_part.

    noun_part(P,G) --> determiner(P,G),noun_form(P,G).

    noun_form(P,G) --> noun(P,G).
    noun_form(P,G) --> noun(P,G),adjective(P,G).

    qualify_part --> qualifier,noun(_,_).
    qualify_part --> qualifier,noun_phrase(_,_).

    verb_phrase(P,G) --> verb(P,G),adv_phrase(P,G).

    adv_phrase(P,G) --> adjective(P,G),qualify_part.

    determiner(s,m) --> [W],{member(W,[le,mon,un])}.
    determiner(s,f) --> [W],{member(W,[la,ma,une])}.
    determiner(p,m) --> [W],{member(W,[les,mes])}.

    noun(s,m) --> [W],{member(W,[trottoir,jardin,pas,lilas])}.
    noun(s,f) --> [rue].
    noun(p,m) --> [jardins].

    adjective(s,m) --> [W],{member(W,[etroit,fleuri])}.
    adjective(s,f) --> [W],{member(W,[etroite,fleurie])}.
    adjective(s,_) --> [sonore].
    adjective(p,m) --> [W],{member(W,[etroits,fleuris])}.

    verb(s,_) --> [est].
    verb(p,_) --> [sont].

    qualifier --> [W],{member(W,[de,dans,a,comme,sur])}.

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

%--------------------------------------------------------------------



