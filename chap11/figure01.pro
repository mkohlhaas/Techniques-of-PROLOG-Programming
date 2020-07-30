% FIGURE 11.1: Simulation model of a single-runway airport
% --------------------------------------------------------------------
% This file contains the simulation model of a single-runway airport.
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following queries to perform simulation runs (If the output does
% not fully show on the screen, press any key to get the next screen):
%   ?- simulate(nosnap).
%   ?- sim_cont(480).
% Note that different runs may produce different results due to the
% generation of random numbers.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
% ====================================================================
% SIMULATION OF A SINGLE RUNWAY AIRPORT
% Time unit = 1 minute
% ====================================================================
object(Time,aircraft,
  [(arrive(I) :-
     do((request(Service,Priority),
         next_arrival(Time,I,NTime,NI))),
     respond([(Time,Priority,runway,queue(aircraft(I))),
        (Time,Priority,runway,seize(aircraft(I):Service)),
        (NTime,np,aircraft,arrive(NI))]))]).

object(Time,runway,
  [(seize(Aircraft:Service) :-
     do((serve_time(Service,ST),NTime is Time + ST)),
     respond([(NTime,np,runway,release(Aircraft))]))]).

request(Service,Priority) :- randomz(P),
    (P < 0.5, Service = takeoff, Priority = 1,!;
              Service = landing, Priority = 0).

next_arrival(CurTime,I,NextTime,NI) :-
    expon(10,AT), NextTime is CurTime + AT,
    NI is I + 1.

serve_time(takeoff,Time) :- normal(15,3,Time).
serve_time(landing,Time) :- normal(20,4,Time).

simulation_time(0,240).
initial_state([free(aircraft),free(runway)]).
start_message([(Time,np,aircraft,arrive(I))]) :-
    next_arrival(0,0,Time,I).
histogram_form((wait-time,runway,(7,0,10))).
