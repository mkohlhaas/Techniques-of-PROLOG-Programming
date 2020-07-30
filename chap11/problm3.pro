% PROBLEM 11.3: Simulation of a fast-food bar
% --------------------------------------------------------------------
% Consult this file and the file \CHAP11\POSS.PRO and use the
% following queries to perform simulation runs:
% (Press any key to get the next screen display).
%   ?- simulate(nosnap).
%   ?- sim_cont(20).
%   ?- sim_cont(30).
%   ?- sim_cont(40).
%   ?- sim_cont(50).
%   ?- sim_cont(60).
% Observe the change in the simulation results.
% Now re-run with the snap indicator on, by using the following query,
% and inspect the snap-shot display. To stop, press <Ctrl-Break>.
%   ?- simulate(snap).
%
% ====================================================================
% SIMULATION OF A FAST-FOOD BAR
% Time unit = 1 minute
% ====================================================================
object(Time,customer,
  [(arrive(I) :-
      do((determine(Queue,Foods),next_arrival(Time,I,NTime,NI))),
      respond([(Time,1,serveq(Queue),queue(customer(I))),
               (Time,1,server(Queue),seize(customer(I):Foods)),
               (NTime,np,customer,arrive(NI))])),
   (arrive(I) :-
      do((restaurant_full,next_arrival(Time,I,NTime,NI))),
      respond([(NTime,np,customer,arrive(NI))])),

   (leave(Cust:N) :-
      do(leave_restaurant),
      respond([(Time,np,serveq(N),leaveq(Cust))]))]).

object(Time,server(N),
  [(seize(Cust:Foods) :-
      do((buffer_plenty(Foods,Time,FTime),register(N,R))),
      respond([(FTime,1,register(R),seize(server(N):Cust))])),
   (seize(Cust:Foods) :-
      do(buffer_low(Foods,Time,WTime)),
      respond([(Time,0,buffer,setoff(server(N))),
         (Time,0,kitchen_signal,seton(server(N))),
         (WTime,0,server(N),release(Cust)),
         (WTime,0,buffer,wait(server(N):(Cust:Foods)))]))]).

object(Time,register(R),
  [(seize(server(N):Cust) :-
      do(pay_bill(Time,FTime)),
      respond([(FTime,np,register(R),release(server(N))),
         (FTime,np,server(N),release(Cust)),   
         (FTime,np,customer,leave(Cust:N))]))]).

object(Time,cook,
  [(supply :-
      do(prepare_time(Time,FTime)),
      respond([(FTime,0,kitchen_signal,wait(cook))]))]).

object(Time,kitchen_signal,
  [(wait(cook) :-
      do(replenish_buffer),
      respond([(Time,0,kitchen_signal,setoff(cook)),
               (Time,0,buffer,seton(cook)),
               (Time,0,cook,supply)]))]).

object(Time,buffer,
  [(wait(Server:Cust) :-
      do(true),
      respond([(Time,0,Server,seize(Cust))]))]).

determine(Queue,(Hamb,Chick,Chips)) :-
    counter(custin_room,N), N < 16, 
    choose_foods((Hamb,Chick,Chips)),
    Hamb + Chick + Chips > 0,
    change_counter(custin_room,1),
    choose_queue(Queue).

choose_foods((Hamb,Chick,Chips)) :-
    uniform(0,5,Hamb),
    uniform(0,6,Chick),
    uniform(0,3,Chips).

choose_queue(N) :-
    shortest_queue([serveq(1),serveq(2),
    serveq(3),serveq(4)],serveq(N),_).

restaurant_full :-
    counter(custin_room,N), N >= 16,!,
    change_counter(lost_customers,1).
restaurant_full.

leave_restaurant :-
    change_counter(custin_room,-1).

next_arrival(Time,I,NTime,NI) :-
    uniform(0,2,AT), NTime is Time + AT,
    NI is I + 1.

buffer_plenty((Hamb,Chick,Chips),Time,FTime) :-
    buffer(Bham,Bchk,Bchp),
    NBham is Bham - Hamb,  NBham >= 0,
    NBchk is Bchk - Chick, NBchk >= 0,
    NBchp is Bchp - Chips, NBchp >= 0,!,
    retract(buffer(Bham,Bchk,Bchp)),
    assert(buffer(NBham,NBchk,NBchp)),
    uniform(3,6,ST), FTime is Time + ST.

buffer_low((Hamb,Chick,Chips),Time,FTime) :-
    buffer(Bham,Bchk,Bchp),
    (Bham < Hamb; Bchk < Chick; Bchp < Chips),!,
    FTime is Time + 1.

register(N,R) :-
    R is (N//3) + 1.

pay_bill(Time,PTime) :-
    PTime is Time + 2.

prepare_time(Time,FTime) :-
    uniform(5,10,PT),
    FTime is Time + PT.

replenish_buffer :-
    retract(buffer(_,_,_)),
    assert(buffer(20,20,30)).

change_counter(Name,I) :-
    retract(counter(Name,N)),
    N1 is N + I,
    assert(counter(Name,N1)).

counter(custin_room,0).
counter(lost_customers,0).
buffer(20,20,30).

simulation_time(0,12).
initial_state([free(customer),free(cook),
    free(register(1)),free(register(2)),
    free(server(1)),free(server(2)),
    free(server(3)),free(server(4)),
    free(buffer)]).
start_message([(0,1,cook,supply),(Time,1,customer,arrive(I))]) :-
    next_arrival(0,0,Time,I).

histogram_form((wait-time,serveq(_),(7,6,3))).
statistic_focus(wait-time,serveq(_)).
statistic_focus(wait-time,register(_)).
statistic_focus(use-time,server(_)).
statistic(wait-time,server(_),nil).
statistic(use-time,register(_),nil).

capacity(server(_),4).
