% FIGURE 10.2: A definite clause grammar
% ---------------------------------------------------------
% consult this file and try the following queries to parse
% the sentences for grammatical structure:
%   ?- sentence([the,president,has,a,cat],[]).
%   ?- sentence([many,cats,have,a,president],[]).
%   ?- sentence(S,[]).
% ---------------------------------------------------------
sentence --> noun_phrase(P),verb_phrase(P).
noun_phrase(P) --> determiner(P),noun(P).
verb_phrase(P) --> verb(P),noun_phrase(_).

determiner(_) --> [the].
determiner(sgular) --> [a].
determiner(plural) --> [many].

noun(sgular) --> [president].
noun(sgular) --> [cat].
noun(plural) --> [cats].

verb(sgular) --> [has].
verb(plural) --> [have].
