% FIGURE 11.12-13: Simulation model of a motor repair shop
%--------------------------------------------------------------------
% This file contains the simulation model of a motor repair shop.
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following query to perform the simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(480).
% Observe the change in the simulation results.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
%====================================================================
% SIMULATION MODEL OF A MOTOR REPAIR SHOP
% Time unit = 1 minute
%====================================================================

object(Time,motor,
   [(arrive(I) :-
        do((need(Service),next_arrival(Time,I,NTime,NI))),
        respond([(Time,1,mechanic(1),seize(motor(I):Service)),
                 (NTime,np,motor,arrive(NI))]))]).
object(Time,mechanic(1),
   [(seize(Motor:Service) :-
        do((repair(Service),
            serve_time(remove-casing,RT),CTime is Time + RT,
            serve_time(Service,ST),WTime is CTime + ST)),
        respond([(CTime,1,mechanic(2),seize(c(Motor):clean-casing)),
        (WTime,1,mechanic(1),waitfor(mechanic(2):c(Motor)))])),
    (waitfor(mechanic(2):c(Motor)) :-
        do((serve_time(paint-casing,PT),FTime is Time + PT)),
        respond([(FTime,np,mechanic(1),release(Motor))])),
    (seize(Engine:assemble) :-
        do((serve_time(assemble,AT),ATime is Time + AT)),
        respond([(ATime,np,mechanic(1),release(Engine))]))]).

object(Time,mechanic(2),
   [(seize(c(Motor):clean-casing) :-
        do((serve_time(clean-casing,CT),WTime is Time + CT)),
        respond([(WTime,1,mechanic(2),waitfor(mechanic(1):Motor))])),
    (waitfor(mechanic(1):Motor) :-
        do((serve_time(clean-engine,ET),ATime is Time + ET)),
        respond([(ATime,np,mechanic(2),release(c(Motor))),
                 (ATime,0,mechanic(1),seize(c(Motor):assemble))]))]).

need(Service) :- randomz(P),
    (P < 0.3, Service = minor-repair,!;
     P < 0.8, Service = major-repair,!;
              Service = adjustment).

next_arrival(CurTime,I,NextTime,NI) :-
    uniform(30,45,AT), NextTime is CurTime + AT,
    NI is I + 1.

repair(Service) :- 
    member(Service,[minor-repair,major-repair,adjustment]).
serve_time(remove-casing,Time) :- uniform(5,10,Time).
serve_time(minor-repair,Time) :- uniform(15,30,Time).
serve_time(major-repair,Time) :- uniform(30,60,Time).
serve_time(adjustment,Time) :- uniform(10,20,Time).
serve_time(paint-casing,Time) :- uniform(10,20,Time).
serve_time(clean-casing,Time) :- uniform(10,30,Time).
serve_time(clean-engine,Time) :- uniform(20,30,Time).
serve_time(assemble,Time) :- uniform(10,20,Time).

simulation_time(0,240).
initial_state([free(motor),free(mechanic(1)),free(mechanic(2))]).
start_message([(Time,np,motor,arrive(I))]) :-
    next_arrival(0,0,Time,I).
histogram_form((wait-time,mechanic(1),(6,20,20))).

%===================================================================


