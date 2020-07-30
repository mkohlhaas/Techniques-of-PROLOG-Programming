% FIGURE 11.3: Simulation of a single-runway airport with two queues
% ------------------------------------------------------------------------
% This file contains the simulation model of a single-runway airport
% with two separate queues for aircraft waiting to land and to take-off.
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following queries to perform simulation runs for a total of eight
% hours (simulated time).
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(400).
%   ?- sim_cont(480).
% Compare the final result with that of the model given in Figure 11.2.
% Re-run the model a number of times, you will find that at some stage
% the simulation result is almost the same as the one produced by the
% model of Figure 11.2.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
% ===========================================================
% SIMULATION OF A SINGLE RUNWAY AIRPORT WITH TWO QUEUES
% Time unit = 1 minute
% ===========================================================
object(Time,aircraft,
    [(arrive(I) :-
       do((request(Service,Priority),
           next_arrival(Time,I,NTime,NI))),
       respond([(Time,Priority,queue(Service),queue(aircraft(I))),
          (Time,Priority,runway,seize(aircraft(I):Service)),
          (NTime,np,aircraft,arrive(NI))]))]).

object(Time,runway,
    [(seize(Aircraft:Service) :-
       do((serve_time(Service,ST),NTime is Time + ST)),
       respond([(Time,0,queue(Service),leaveq(Aircraft)),
                (NTime,np,runway,release(Aircraft))]))]).

request(Service,Priority) :- randomz(P),
    (P < 0.5, Service = takeoff, Priority = 1,!;
              Service = landing, Priority = 0).

next_arrival(CurTime,I,NextTime,NI) :-
    expon(10,AT), NextTime is CurTime + AT,
    NI is I + 1.

serve_time(takeoff,Time) :- normal(15,3,Time).
serve_time(landing,Time) :- normal(20,4,Time).

simulation_time(0,200).
initial_state([free(aircraft),free(runway)]).
start_message([(Time,np,aircraft,arrive(I))]) :-
    next_arrival(0,0,Time,I).
histogram_form((wait-time,runway,(7,0,10))).
statistic(wait-time,runway,nil).
