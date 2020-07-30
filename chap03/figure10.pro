% FIGURE 3.10: Effect of goals order
% ---------------------------------------------------
% consult this program and enter the following query:
%   ?- write_list([l,i,v,e,d]).
% Now, exchange the goals write(H) and write_list(T), then
% try the above query again. Observe the output.
% ---------------------------------------------------
write_list([]).
write_list([H|T]) :-
    write(H),
    write_list(T).
