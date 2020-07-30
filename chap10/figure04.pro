% FIGURE 10.4: A definite clause grammar that generates parse trees
% -------------------------------------------------------------------
% consult this file and try the following queries to parse sentences
% and to obtain the parse trees representing their grammar structure:
%   ?- sentence(S,[the,president,has,a,cat],[]).
%   ?- sentence(S,[many,cats,have,a,president],[]).
%   ?- restricted_sentence(S).
% -------------------------------------------------------------------
sentence(s(NP,VP)) -->
    noun_phrase(P,NP),verb_phrase(P,VP).
noun_phrase(P,np(D,N)) -->
    determiner(P,D),noun(P,N).
verb_phrase(P,vp(V,NP)) -->
    verb(P,V),noun_phrase(_,NP).

determiner(_,dt(the)) --> [the].
determiner(sgular,dt(a)) --> [a].
determiner(plural,dt(many)) --> [many].

noun(sgular,n(president)) --> [president].
noun(sgular,n(cat)) --> [cat].
noun(plural,n(cats)) --> [cats].

verb(sgular,v(has)) --> [has].
verb(plural,v(have)) --> [have].

restricted_sentence(S) :-
  sentence(s(np(dt(the),n(N1)),vp(_,np(dt(D),n(N2)))),S,[]),
  D \= the, N1 \= N2.
