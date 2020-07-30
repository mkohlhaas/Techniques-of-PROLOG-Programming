% FIGURE 6.19: Generating news stories using frames
% ------------------------------------------------------------------
% consult this program and enter the following query:
%   ?- create_frame(earthquake,Frame),
%      news(earthquake,Frame,News),
%      nl,print_news(News).
% then fill in the frame by entering details, e.g.
%   place : newcastle.
%   day : today.
%   time : 5-am.
%   killed : 8.
%   injured : X.
%   damage : 2-million.
%   magnitude : 6.5.
% Now, create the frames for war, election, royal_wedding, etc..
% in the form of news(earthquate,[...]), and enter queries
% similar to the above to generate news stories on war, election,...
% ------------------------------------------------------------------
:- op(500,xfx,has).
:- op(500,xfx,isa).
:- op(500,xfy,:).

% EVENT              FRAME
event has            [place: _, day: _, time: _].
disaster_event has   [killed: _, injured: _, damage: _].
political_event has  [who: _, what: _].
sport_event has      [sport: _, winner: _, score: _].
earthquake has       [magnitude: _].

disaster_event isa event.
political_event isa event.
sport_event isa event.
earthquake isa disaster_event.

create_frame(Event,Frame) :-
    bagof(Slot,collect_slot(Event,Slot),Frame),
    fillup(Frame).

collect_slot(Event,Slot) :-
    Event isa Event1, collect_slot(Event1,Slot).
collect_slot(Event,Slot) :-
    Event has Frame, member(Slot,Frame).

fillup([]) :- nl.
fillup([Slot:Filler|Rest]) :-
    write(Slot),write(' : '),
    read(Filler),
    fillup(Rest).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% FIGURE 6.20: Conversion of a frame into a news-story
% ----------------------------------------------------------
news(earthquake,[place:P, day:D, time:T, killed:K,
                 injured:I, damage:Da, magnitude:M],
  ['An',earthquake,occurred,in,P,D,at,T,nl,measured,at,M,
   on,the,'Richter',scale,'.','There',were,K,killed,',',nl,
   I,injured,and,damage,estimated,at,'$',Da,'.']).

print_news([]) :- nl.
print_news([W|Rest]) :-
    (var(W),write('unknown ');
     nonvar(W),
        (W = nl,!,nl; write(W),write(' '))),
    print_news(Rest).
