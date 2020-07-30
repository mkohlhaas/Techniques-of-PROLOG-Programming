% FIGURE 4.3: The deterministic program bubble-sort
%------------------------------------------------------
% consult this program and try the following query:
%   ?- bubble_sort([3,1,2],L).
% Now, remove the cut after the goal swap(L,L2,0),
% then try the above query again, using ; to request
% alternative answers. Observe and explain the results.
%------------------------------------------------------
    bubble_sort(L,L1) :- swap(L,L2,0),!,
                         bubble_sort(L2,L1).
    bubble_sort(L,L).

    swap([X,Y|R],[X|T],S) :- X =< Y,!,swap([Y|R],T,S).
    swap([X,Y|R],[Y|T],S) :- X > Y,!,swap([X|R],T,1).
    swap([X],[X],1).



