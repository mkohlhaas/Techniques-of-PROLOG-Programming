% FIGURE 1.2: The world of Ann and Sue (version 2)
%----------------------------------------------------
% consult this program and try the following queries,
% use ; to request alternative answers.
%   ?- likes(sue,doll).
%   ?- likes(sue,snoopy).
%   ?- likes(sue,X).
%   ?- toy(X).
%   ?- plays(sue,X).
%   ?- plays(ann,X).
%   ?- toy(X),likes(sue,X).
%----------------------------------------------------
    likes(ann,X) :- toy(X),plays(ann,X).
    toy(doll).
    toy(snoopy).
    plays(ann,doll).
    plays(ann,snoopy).
    likes(sue,X) :- likes(ann,X).

