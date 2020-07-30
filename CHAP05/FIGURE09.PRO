% FIGURE 5.9: A program to paint a map
%---------------------------------------------------------------
% consult this program and try the following queries:
%  ?- paint('Australia',[red,blue,yellow,purple],Coloured_Map).
%  ?- paint('West Europe',[blue,orange,green,red],Coloured_Map).
%---------------------------------------------------------------
:- op(500,xfy,:).

  paint(MapName,Colours,ColouredMap) :-
      map(MapName,StateList),
      arrange_colours(StateList,Colours,[],ColouredMap).

  arrange_colours([S:Nbs|Rest],Colours,ChosenColours,Result) :-
      member(C,Colours),
      different(C,Nbs,ChosenColours),
      arrange_colours(Rest,Colours,[S:C|ChosenColours],Result).
  arrange_colours([],_,Result,Result).

  different(C,Nbs,[S1:C1|Rest]) :-
      not((member(S1,Nbs), C = C1)),
      different(C,Nbs,Rest).
  different(_,_,[]).

  member(X,[X|_]).
  member(X,[H|T]) :- member(X,T).

  map('Australia',
     [wa : [nt,sa], nt: [wa,sa,qld], qld: [nt,sa,nsw],
      nsw: [qld,sa,vic], vic: [sa,nsw], tas: [],
      sa : [wa,nt,qld,nsw,vic]]).

  map('West Europe',
     [portugal : [spain],
      spain    : [portugal,france],
      belgium  : [france,holland,luxembrg,germany],
      holland  : [belgium,germany],
      luxembrg : [france,belgium,germany],
      switzerld: [france,germany,austria,italy],
      italy    : [france,switzerld,austria],
      austria  : [germany,switzerld,italy],
      france   : [spain,belgium,luxembrg,germany,
                  switzerld,italy],
      germany  : [holland,belgium,luxembrg,france,
                  switzerld,austria]]).


