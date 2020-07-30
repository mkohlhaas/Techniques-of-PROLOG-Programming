% PROBLEM 2.9: A database for domestic airflights in Australia
% -------------------------------------------------------------
% consult this program and try the following queries:
%   ?- depart_time(Time,canberra,sydney,ansett,wed).
%   ?- depart_tim2(Time,canberra,sydney,ansett,wed).
% extend the database to cover more flights and try the queries
% similar to the ones given above.
% -------------------------------------------------------------
:- op(500,xfy,:).

% FLIGHT(DEPART,ARRIVAL,
%       [(DEPTIME,ARRTIME,FLIGHTNUM,AIRLINE,DAYS),...])
flight(canberra,sydney,
      [( 7:45, 8:35, 41,eastwest,[mon,fri]),
       (10:40,11:30,184,ansett,alldays),
       (14:00,14:50, 48,australian,[mon,wed,fri])]).

flight(sydney,brisbane,
      [( 8:45,10:45, 42,easwest,[mon,tue,thu,fri]),
       (11:50,13:40,186,ansett,alldays),
       (15:10,17:10, 50,australian,[mon,wed,fri])]).

available(_,alldays).
available(Day,AvailList) :- member(Day,AvailList).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

depart_time(DepTime,Depart,Destination,Airline,Day) :-
    flight(Depart,Destination,FlightList),
    member((DepTime,_,_,Airline,AvailList),FlightList),
    available(Day,AvailList).


% FLIGHTS AND SCHEDULES DATABASE
flight2(canberra,sydney,table1).
flight2(sydney,brisbane,table2).

schedule(table1, 7:45, 8:35, 41,easwest,[mon,fri]).
schedule(table1,10:40,11:30,184,ansett,alldays).
schedule(table1,14:00,14:50, 48,australian,[mon,wed,fri]).
schedule(table2, 8:45,10:45, 42,eastwest,[mon,tue,thu,fri]).
schedule(table2,11:50,13:40,186,ansett,alldays).
schedule(table2,15:10,17:10, 50,australian,[mon,wed,fri]).

depart_tim2(DepTime,Depart,Destination,Airline,Day) :-
    flight2(Depart,Destination,Table),				% typo Depart, Detpart?
    schedule(Table,DepTime,_,_,Airline,AvailList),
    available(Day,AvailList).
