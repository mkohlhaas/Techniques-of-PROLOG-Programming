% FIGURE 11.9-10: Simulation model of a ship-dock system
%--------------------------------------------------------------------
% This file contains the simulation model of a ship-dock system.
% Consult this file and consult the file \CHAP11\POSS.PRO, then enter
% the following queries to perform the simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(400).
%   ?- sim_cont(480).
% Observe the change in the simulation results.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
%====================================================================
% SIMULATION MODEL OF A SHIP-DOCK SYSTEM
% Time unit = 1 minute
%====================================================================

object(Time,ship,
    [(arrive(I) :-
       do((ship_type(Type,BerthTugs),
           next_arrival(Time,I,NTime,NI))),
       respond([(Time,np,BerthTugs,seize(ship(I):Type)),
                (NTime,np,ship,arrive(NI))]))]).

object(Time,berth(Type:N),
    [(seize(Ship:Type) :-
       do((berth_time(Type,BT),LTime is Time + BT)),
       respond([(LTime,np,berth(Type:N),release(Ship))]))]).

object(Time,tug(N),
    [(seize(Ship:Type) :-
       do((tug_time(Type,TT),FTime is Time + TT)),
       respond([(FTime,np,tug(N),release(Ship))]))]).

ship_type(Type,BerthTugs) :-
    randomz(P),
    (P < 0.4,!,
     Type = large, BerthTugs = (berth(large:N),tug(I),tug(J));
     Type = small, BerthTugs = (berth(small:N),tug(I))).

next_arrival(CurTime,I,NextTime,NI) :-
    uniform(10,30,AT), NextTime is CurTime + AT,
    NI is I + 1.

berth_time(large,Time) :- uniform(60,120,Time).
berth_time(small,Time) :- uniform(40,90,Time).

tug_time(large,Time) :- uniform(20,30,Time).
tug_time(small,Time) :- uniform(15,20,Time).

simulation_time(0,200).
initial_state(States) :-
    setup_berths(2,large,[free(ship)],AStates),
    setup_berths(3,small,AStates,BStates),
    setup_tugs(3,BStates,States).
start_message([(Time,np,ship,arrive(I))]) :-
    next_arrival(0,0,Time,I).

histogram_form((wait-time,(berth(large:_),tug(_),tug(_)),(7,0,5))).
histogram_form((wait-time,(berth(small:_),tug(_)),(7,0,5))).

statistic_focus(wait-time,(berth(large:_),tug(_),tug(_))).
statistic_focus(wait-time,(berth(small:_),tug(_))).
statistic_focus(use-time,berth(large:_)).
statistic_focus(use-time,berth(small:_)).
statistic(use-time,tug(_),nil).

capacity(berth(large:_),2).
capacity(berth(small:_),3).

setup_berths(0,_,States,States) :- !.
setup_berths(N,Type,States,NStates) :-
    N1 is N - 1,
    setup_berths(N1,Type,[free(berth(Type:N))|States],NStates).

setup_tugs(0,States,States) :- !.
setup_tugs(N,States,NStates) :-
    N1 is N - 1,
    setup_tugs(N1,[free(tug(N))|States],NStates).

%------------------------------------------------------------


