% FIGURE 11.4: Simulation of a two-runway airport with two queues
%----------------------------------------------------------------------
% This file contains the simulation model of a two-runways airport
% with two separate queues for aircraft waiting to land and to take-off
% (They wait for any of the runways to be available).
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following queries to perform the simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(160).
%   ?- sim_cont(240).
%   ?- sim_cont(320).
%   ?- sim_cont(400).
%   ?- sim_cont(480).
% Observe the change in the simulation results. Compare the final result
% with that of the model given in Figure 11.3.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
%====================================================================
% SIMULATION OF AN AIRPORT  WITH TWO RUNWAYS AND
% TWO SEPARATE QUEUES FOR LANDING AND TAKING-OFF
%      Time unit = 1 minute
%====================================================================

object(Time,aircraft,
    [(arrive(I) :-
       do((request(Service,Priority),
           next_arrival(Time,I,NTime,NI))),
       respond([(Time,Priority,queue(Service),queue(aircraft(I))),
          (Time,Priority,runway(_),seize(aircraft(I):Service)),
          (NTime,np,aircraft,arrive(NI))]))]).

object(Time,runway(N),
    [(seize(Aircraft:Service) :-
       do((serve_time(Service,ST),NTime is Time + ST)),
       respond([(Time,0,queue(Service),leaveq(Aircraft)),
                (NTime,np,runway(N),release(Aircraft))]))]).

request(Service,Priority) :- randomz(P),
    (P < 0.5, Service = takeoff, Priority = 1,!;
              Service = landing, Priority = 0).

next_arrival(CurTime,I,NextTime,NI) :-
    expon(10,AT), NextTime is CurTime + AT,
    NI is I + 1.

serve_time(takeoff,Time) :- normal(15,3,Time).
serve_time(landing,Time) :- normal(20,4,Time).

simulation_time(0,80).
initial_state([free(aircraft),free(runway(1)),free(runway(2))]).
start_message([(Time,np,aircraft,arrive(I))]) :-
    next_arrival(0,0,Time,I).
histogram_form((wait-time,runway(_),(7,0,5))).
statistic_focus(wait-time,runway(_)).
statistic(wait-time,runway(_),nil).

%------------------------------------------------------------




