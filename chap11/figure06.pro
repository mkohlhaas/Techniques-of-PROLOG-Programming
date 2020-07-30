% FIGURE 11.6-7: Simulation model of a simulated journey on Mars
% -----------------------------------------------------------------------
% This file contains the simulation model of a simulated journey on Mars.
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following queries to perform a simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(40).
%   ?- sim_cont(60).
%   ?- sim_cont(80).
%   ?- sim_cont(100).
%   ?- sim_cont(120).
% Observe the change in the simulation results.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
% ======================================================================
% SIMULATION MODEL OF A SIMULATED JOURNEY ON MARS
% Time unit = 1 miniute
% ======================================================================
object(Time,passenger,
    [(arrive(I) :-
       do((buy(Tickets),next_arrival(Time,I,NTime,NI))),
       respond([(Time,np,vehicle(_),queue(pass(I))),
                (Time,np,vehicle(_),seize(pass(I):Tickets)),
                (NTime,np,passenger,arrive(NI))]))]).
object(Time,vehicle(V:N),
    [(seize(Pass:2) :-
       do((spend_time(V,ST),LTime is Time + ST,
           next_vehicle(V,NV))),
       respond([(LTime,np,vehicle(V:N),release(Pass)),
                (LTime,np,vehicle(_),queue(Pass)),
                (LTime,np,vehicle(NV:_),seize(Pass:1))])),
     (seize(Pass:1) :-
       do((spend_time(V,ST),LTime is Time + ST)),
       respond([(LTime,np,vehicle(V:N),release(Pass))]))]).

buy(Tickets) :- uniform(1,2,Tickets).

next_arrival(CurTime,I,NextTime,NI) :-
    uniform(0,3,AT), NextTime is CurTime + AT,
    NI is I + 1.

next_vehicle(boat,jeep).
next_vehicle(jeep,boat).

spend_time(boat,Time) :- uniform(15,25,Time).
spend_time(jeep,Time) :- uniform(10,20,Time).

simulation_time(0,20).
initial_state(States) :-
    setup(3,boat,[free(passenger)],BStates),
    setup(4,jeep,BStates,States).

start_message([(Time,np,passenger,arrive(I))]) :-
    next_arrival(0,0,Time,I).

histogram_form((wait-time,vehicle(_),(7,0,10))).

statistic_focus(wait-time,vehicle(_)).
statistic_focus(use-time,vehicle(boat:_)).
statistic_focus(use-time,vehicle(jeep:_)).
statistic_focus(queue,vehicle(_)).

capacity(vehicle(boat:_),3).
capacity(vehicle(jeep:_),4).

setup(0,_,States,States) :- !.
setup(N,V,States,FStates) :-
    N1 is N - 1,
    setup(N1,V,[free(vehicle(V:N))|States],FStates).
