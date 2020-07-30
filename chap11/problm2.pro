% PROBLEM 11.2: Simulation of a computer multiplexor using gates
% --------------------------------------------------------------
% Consult this file and the file \CHAP11\POSS.PRO and use the
% following queries to perform simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(2000).
%   ?- sim_cont(3000).
%      ...
%   ?- sim_cont(18000).
% Observe the change in the simulation results.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
% ============================================================
% SIMULATION OF A COMPUTER MULTIPLEXER USING GATES
%     Time unit = 100th of second
% ============================================================
object(Time,message,
  [(arrive(I) :-
      do((terminals(N),message_type(Type),
          next_arrival(Time,I,NTime,NI))),
      respond([(Time,1,queue(N),queue(message(I))),
               (Time,1,stngate(N),wait(message(I):Type)),
               (NTime,1,message,arrive(NI))]))]).

object(Time,poller,
  [(poll(I) :-
      do((poll_station(I,N),poll_time(Time,PTime,CTime))),
      respond([(PTime,1,stngate(N),seton(poller(I))),
               (CTime,0,stngate(N),wait(poller(I):_))]))]).

object(Time,stngate(N),
  [(wait(message(I):Type) :-
      do(true),
      respond([(Time,0,stngate(N),setoff(message(I))),
               (Time,1,queue(N),leaveq(message(I))),
               (Time,1,station(N),seize(message(I):Type))])),
   (wait(poller(I):_) :-
      do(I1 is (I mod 14) + 1),
      respond([(Time,0,stngate(N),setoff(poller(I))),
               (Time,1,poller,poll(I1))]))]).

object(Time,station(N),
  [(seize(message(I):Type) :-
      do(transmit_time(Type,Time,FTime)),
      respond([(FTime,1,station(N),release(message(I))),
               (FTime,1,stngate(N),seton(message(I)))]))]).

next_arrival(Time,I,NTime,NI) :-
    expon(300,AT), NTime is Time + AT,
    NI is I + 1.

terminals(N) :-
    randomz(P),
    (P < 0.143,!, N is 1;
     P < 0.457,!, N is 2;
     P < 0.670,!, N is 3;
                  N is 4).

message_type((Type,Length)) :-
    randomz(P),
    (P < 0.2,!, Type = enquiry, Length = 50;
     Type = data, randomz(Q),
     (Q < 0.1,!, Length = 30;
      Q < 0.8,!, Length = 50;
                 Length = 80)).

transmit_time((enquiry,Length),Time,FTime) :-
    uniform(50,100,PT),
    FTime is Time + 5*(Length + 50) + PT.
transmit_time((data,Length),Time,FTime) :-
    FTime is Time + 5*(Length + 20) + Length.

poll_station(I,N) :-
    member((I:N),
    [1:1,2:2,3:4,4:3,5:4,6:2,7:4,8:1,9:3,10:4,11:2,12:4,13:3,14:4]).

poll_time(Time,PTime,CTime) :-
    PTime is Time + 50,
    CTime is PTime + 1.

simulation_time(0,1000).
initial_state([free(poller),free(message),
    free(station(1)),free(station(2)),
    free(station(3)),free(station(4))]).
start_message([(0,1,poller,poll(1)),(Time,1,message,arrive(I))]) :-
    next_arrival(0,0,Time,I).

histogram_form((wait-time,queue(4),(7,0,100))).
statistic(_,stngate(_),nil).
statistic(wait-time,station(_),nil).
