% FIGURE 10.1: A context-free grammar
%----------------------------------------------------------
% consult this file and try the following queries to parse
% the sentences for grammatical structure:
%   ?- sentence([the,president,has,a,cat],[]).
%   ?- sentence([the,president,has,a,cat,suit],R).
%   ?- sentence([the,president,has,a,wild,cat],R).
%   ?- noun([wild,cat],R).
%   ?- sentence(S,[]).
%----------------------------------------------------------
    sentence --> noun_phrase, verb_phrase.
    noun_phrase --> determiner, noun.
    verb_phrase --> verb, noun_phrase.

    determiner --> [the].
    determiner --> [a].

    noun --> [president].
    noun --> [cat].

    verb --> [has].

%----------------------------------------------------------



