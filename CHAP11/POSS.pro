% FIGURE 11.14-18: POSS.
%------------------------------------------------------------------
% This file contains the complete POSS system that executes the
% simulation models of the form described in Section 11.3.
%
%==================================================================
% POSS: A PROLOG-BASED OBJECT-ORIENTED SIMULATION SYSTEM
%==================================================================
% NOTES:
%  1. Queues lengths are processed only if the model contains a
%     message of the form:
%        (Time,Priority,QName,queue(Message)).
%     Histogram is established only if the model contains a clause:
%        histogram_form((Var,Object,(NBars,Low,Interv))).
%
%  2. Wait time and Use time statistics are always collected unless
%     the model indicates:
%        statistic(Time,Object,nil).
%     If there are several objects of the same kind and we want to
%     collect their overall statistics, then we must specify in the
%     model:
%        statistic_focus(Time,Object(_)).
%     Otherwise, individual statistics are collected separately.
%
%  3. There are only 8 predefined messages (see page 537 for details):
%     queue(M) : update the specified queue; does not change the list
%         of scheduled messages, States and other Statistics.
%     leaveq(M) : update the specified queue, collect waiting time;
%         everything else unchanged.
%     seize(M:W) : wait until an object (or group of objects) is free,
%         then occupy the object (which becomes busy) and add its
%         responses to the list of scheduled messages; wait time is
%         collected, queue decremented.
%     release(M) : free specified object; collect use time.
%     waitfor(O:M) : wait until meet another object, then both are
%         sent; the list of scheduled messages is changed; no statistics
%         is collected.
%     dropout(M) : message leaves queue, abandoning process; the waiting
%         message is removed, everything else unchanged.
%     seton(M) : makes the specified object free (States change);
%         anything else unchanged.
%     setoff(M): makes the specified object unavailable (States change);
%         anything else unchanged.
%
%  4. There are two built-in functions involving queues:
%     queue_length(Q,N) : provides the length N of queue Q.
%     shortest_queue(QList,Q,N) : provides the shortest queue Q with
%                    length N in the list of queues QList.
%
%  5. There are three statistical functions:
%     uniform(A,B,X) : uniform distribution in the range A-B.
%     expon(M,X)     : negative exponential distribution of mean M.
%     normal(M,S,X)  : normal distribution of mean M and sdt dev S.
%
%  6. The predicates "randomz" and "roundup" are used in this system
%     to generate random numbers (between 0 and 1) and to round a
%     floating-point number to its nearest integer. Suppose that your
%     Prolog system provides a function "rand(X)" that yields a
%     random real number between 0 and X, then you must redefine
%     "randomz" by using the following command (note "asserta"!!!):
%        ?- asserta((randomz(X) :- X is rand(1),!)).
%     Also, suppose your Prolog system has a function "floor(X)" (or
%     something like that) that returns the integer value of X, then
%     use the following command to redefine the predicate "roundup":
%        ?- asserta((roundup(X,Y) :- Y is floor(X+0.5),!)).
%
%======================================================================
% TOP LEVEL CONTROL
%======================================================================
:- op(500,xfx,isa).
:- op(500,xfy,:).

simulate(Snap) :- 
    reset,
    simulation_time(Start,Stop),initial_state(States),
    start_message(Messs),histogram_specification(Hist),
    simul_run((Start,Stop,Messs,States,Hist,[],Snap)).

simul_run((CT,ST,Messs,States,Hist,Statistics,Snap)) :-
    CT >= ST,!,
    store((CT,ST,Messs,States,Hist,Statistics,Snap)),
    snap_shot(Snap,CT,Messs,States,Hist,Statistics),
    report(CT,Hist,Statistics).
simul_run((CT,ST,Messs,States,Hist,Statistics,Snap)) :-
    snap_shot(Snap,CT,Messs,States,Hist,Statistics),
    select_message(CT,Mess,Messs,RMesss,States),!,
    send_message(CT,Mess,RMesss,States,Hist,Statistics,
        NxT,NMesss,NStates,NHist,NStatistics),
    simul_run((NxT,ST,NMesss,NStates,NHist,
        NStatistics,Snap)).

select_message(CT,Mess,Messs,RMesss,States) :-
    member((T,P,O,M),Messs),
    (available(O,O1,States); general_message(M)),!,
    select_mess(CT,(T,P,O,M),O1,Mess,Messs,RMesss).

available((O,Os),(O1,Os1),States) :- !,
    select(free(O1),States,NStates),not(not(O = O1)),!,
    available(Os,Os1,NStates).
available(O,O1,States) :-
    member(free(O1),States),not(not(O = O1)).

select_mess(CT,(T,P,O,M),O,(T,P,O,M),Messs,RMesss) :-
    (CT =< T; P = np),!,
    select((T,P,O,M),Messs,RMesss).
select_mess(CT,_,O,(T1,P1,O1,M1),Messs,RMesss) :-
    findall((P,T,Ob,M),(among(Ob,O),member((T,P,Ob,M),Messs),
        T =< CT),List), sort(List,[(P1,T1,O1,M1)|_]),
    select((T1,P1,O1,M1),Messs,RMesss).

send_message(CT,(ET,POM),Messs,States,Hist,Statistics,
    NT,NMesss,NStates,NHist,NStatistics) :-
    max(CT,ET,NT),
    update_states(NT,PT,(ET,POM),Messs,States,NMesss,NStates),
    update_statis(NT,PT,POM,Hist,Statistics,NHist,NStatistics).


%---------------------------------------------------------------------
% UPDATE THE OBJECTS STATES
%---------------------------------------------------------------------
update_states(CT,ET,(ET,_,_,queue(_)),Messs,States,Messs,States) :- !.
update_states(CT,WT,(_,_,_,leaveq(_)),Messs,States,Messs,States) :- !.
update_states(CT,ET,(ET,_,O,seton(_)),Messs,States,Messs,NStates) :- !,
    (member(free(O),States),!,NStates = States;
     NStates = [free(O)|States]).
update_states(CT,ET,(ET,_,O,setoff(_)),Messs,States,Messs,NStates) :- !,
    (select(free(O),States,NStates),!; States = NStates).
update_states(CT,ET,(ET,_,O,dropout(M)),Messs,States,NMesss,States) :- !,
    (select((T,P,O,M),Messs,NMesss),T =< ET,!,
     leave_queue(CT,(T,P,O,M)); Messs = NMesss).
update_states(CT,ET,(ET,_,(O,Os),seize(M:W)),Messs,States,
    NMesss,NStates) :- !,
    update_states(CT,ET,(ET,_,O,seize(M:W)),Messs,States,
    MMesss,MStates),update_states(CT,ET,(ET,_,Os,seize(M:W)),
    MMesss,MStates,NMesss,NStates).
update_states(CT,ET,(ET,_,O,seize(M:W)),Messs,States,
    NMesss,[busy(O,M,CT)|NStates]) :- !,
    send(CT,O,seize(M:W),Responses),
    insert_list(Responses,Messs,NMesss),
    select(free(O),States,NStates).
update_states(CT,BT,(_,_,O,release(M)),Messs,States,
    Messs,[free(O)|NStates]) :- !,
    select(busy(O,M,BT),States,NStates).
update_states(CT,ET,(ET,_,O,waitfor(O1:M)),Messs,States,
    NMesss,NStates) :-
    select(wait(O1,O,M1),States,NStates),!,
    send(CT,O,waitfor(O1:M),Resp),
    insert_list(Resp,Messs,MMesss),
    send(CT,O1,waitfor(O:M1),Resp1),
    insert_list(Resp1,MMesss,NMesss).
update_states(CT,ET,(ET,_,O,waitfor(O1:M)),Messs,States,
    Messs,[wait(O,O1,M)|States]) :- !.
update_states(CT,ET,(ET,_,O,M),Messs,States,NMesss,States) :-
    send(CT,O,M,Responses),
    insert_list(Responses,Messs,NMesss).

%------------------------------------------------------------------------
% UPDATE STATISTICS
%------------------------------------------------------------------------
update_statis(CT,ET,(P,O,queue(M)),Hist,Statistics,Hist,Statistics) :- !,
    (queue(_,O,_,_,_),!;
     (statistic_focus(queue,O1), not(not(O = O1)),!; O = O1),
      assert(queue(0,O1,0,0,[]))),
    update_queue(CT,ET,O,(P,ET,M),1).
update_statis(CT,WT,(_,O,M),Hist,Statistics,NHist,NStatistics) :-
    (M = leaveq(A); M = seize(A:W)),!,
    update_queue(CT,WT,O,(_,WT,A),-1),
    update_histogram(CT,WT,(wait-time,O),Hist,NHist),
    update_time(wait-time,CT,WT,O,Statistics,NStatistics).
update_statis(CT,BT,(_,O,release(_)),Hist,Statistics,
    NHist,NStatistics) :- !,
    update_histogram(CT,BT,(use-time,O),Hist,NHist),
    update_time(use-time,CT,BT,O,Statistics,NStatistics).
update_statis(_,_,_,Hist,Statistics,
    Hist,Statistics) :- !.

update_time(Time,CT,PT,O,St,St) :-
    statistic(Time,O,nil),!.
update_time(Time,CT,PT,O,St,[(Time,O1,NTTime,N1)|RSt]) :-
    select((Time,O,TTime,N),St,RSt),!,
    (statistic_focus(Time,O1),not(not(O = O1)),!; O = O1),
    NTTime is TTime + CT - PT,
    N1 is N + 1.
update_time(Time,CT,PT,O,St,[(Time,O1,TTime,1)|St]) :-
    (statistic_focus(Time,O1),not(not(O = O1)),!; O = O1),
    TTime is CT - PT.

update_histogram(CT,PT,(T,O),Hist,[(T,O1,Form1)|NHist]) :-
    select((T,O,Form),Hist,NHist),!,
    histogram(CT,PT,(T,O,Form),(T,O1,Form1)).
update_histogram(_,_,_,Hist,Hist).

histogram(U,V,(T,O,(Freq,A,D)),(T,O1,(NFreq,A,D))) :-
    (statistic_focus(T,O1),not(not(O = O1)),!; O = O1),
    X is (U - V - A) // D,
    tally(X,Freq,NFreq).

tally(X,[F|Fs],[F1|Fs]) :-
    (X =< 0; Fs = []),!,F1 is F + 1.
tally(X,[F|Fs],[F|NFs]) :- !,
    X > 0, X1 is X - 1, tally(X1,Fs,NFs).
tally(X,N,NFreq) :-
    make_hist(N,Freq),tally(X,Freq,NFreq).

make_hist(0,[]) :- !.
make_hist(N,[0|Rest]) :-
    N1 is N-1, make_hist(N1,Rest).

%--------------------------------------------------------------------
% QUEUES PROCESSING
%--------------------------------------------------------------------
update_queue(CT,WT,QName,Mess,I) :-
    retract(queue(PT,QName,TW,N,Queue)),!,
    TW1 is TW + N*(CT - PT), N1 is N + I,
    (statistic_focus(queue,QName1),not(not(QName = QName1)),!;
     QName = QName1),
    change_queue(I,Mess,Queue,Queue1),
    assert(queue(CT,QName1,TW1,N1,Queue1)).
update_queue(_,_,_,_,_).

change_queue(1,Mess,Queue,Queue1) :- !,
    insert(Mess,Queue,Queue1).
change_queue(-1,Mess,Queue,Queue1) :-
    select(Mess,Queue,Queue1),!.

leave_queue(CT,(T,P,O,seize(A:W))) :- !,
    update_queue(CT,WT,O,(P,T,A),-1).
leave_queue(_,_).

queue_length(Queue,N) :-
    queue(_,Queue,_,N,_).

shortest_queue(QList,Queue,0) :-
    member(Queue,QList),not(queue(_,Queue,_,_,_)),!.
shortest_queue(QList,Queue,Len) :-
    setof((N,Q),(T,TW,QM)^(member(Q,QList),queue(T,Q,TW,N,QM)),
          [(Len,Queue)|_]).

general_message(M) :-
    member(M,[queue(_),leaveq(_),seton(_),setoff(_),
              leave(_),release(_),waitfor(_)]).

histogram_specification(HistList) :-
    findall(Hist,histogram_form(Hist),HistList).

%---------------------------------------------------------------
% DUMMY INFORMATION
%---------------------------------------------------------------
histogram_form.
statistic.
statistic_focus.
queue.
capacity.
counter.

%---------------------------------------------------------------
% OBJECT-ORIENTED PROGRAMMING
%---------------------------------------------------------------
send(Time,Object,Message,Responses) :-
    obtain(Time,Object,Methods),
    apply(Message,Methods,Responses).

obtain(Time,Object,Methods) :-
    inherit(Object,AncObj),
    object(Time,AncObj,Methods).

apply(A,Methods,Responses) :-
    member((A :- do(B),respond(Responses)),Methods),
    call(B).

inherit(Object,Object).	      % Depth-first search
inherit(Object,AncObj) :-
    Object isa SupObj,
    inherit(SupObj,AncObj).

%-------------------------------------------------------------
% OUTPUT RESULT
%-------------------------------------------------------------
report(Time,Hist,Statistics) :-
    print_histograms(Hist), 
    print_stat(Time,Statistics),
    print_queues,
    print_counters.

%---------------------------------------------------
print_histograms([]) :- !.
print_histograms([H|Hs]) :-
    print_hist(20,H),
    print_histograms(Hs).

print_hist(_,(T,O,(Freq,_,_))) :-
    integer(Freq),!,
    write_list(['No entries for	',T,' of ',O]),nl,nl.
print_hist(0,(T,O,(Freq,A,D))) :- !,
    length(Freq,N), M is N + 2,
    base_line(M,'.....'), mark_line(0,N,A,D),
    conjterm(T,Term),
    write_list(['Frequency of ',T,Term,O]),nl,
    get0(C),nl.
print_hist(K,(T,O,(Freq,A,D))) :-
    write('     '), print_bar(K,Freq),
    K1 is K - 1, print_hist(K1,(T,O,(Freq,A,D))).

base_line(0,_) :- !,nl, write('     ').
base_line(N,X) :-
    write(X), N1 is N-1, base_line(N1,X).

mark_line(N,N,_,_) :- nl,nl,!.
mark_line(I,N,A,D) :- K is A + I*D, write(K),
    (K < 10,!,write('    ');
     K < 100,!,write('   '); write('  ')),
    I1 is I+1, mark_line(I1,N,A,D).

print_bar(_,[]) :- !,nl.
print_bar(K,[V|Vs]) :-
    (K =< V,!,write('...  '); write('     ')),
    print_bar(K,Vs).

%  inherit(Object,AncObj) :-  % Topo-search
%     topo_sort(Object,Ancestors),
%     member(AncObj,Ancestors).

% ----------------------------------------------------------

print_stat(_,[]) :- !.
print_stat(Time,[(use-time,O,TTime,N)|Rest]) :- !,
    (capacity(O,C),!; C = 1),
    Ratio is (100*TTime//C)//Time,
    write_list(['Utilization of ',O,' is ',Ratio,'%']),nl,
    average((use-time,O,TTime,N)),
    print_stat(Time,Rest).
print_stat(Time,[Stat|Rest]) :-
    average(Stat),
    print_stat(Time,Rest).

average((Time,O,TTime,N)) :-
    (N = 0,!, AV = 0; AV is TTime // N), 
    conjterm(Time,Term),
    write_list(['Average ',Time,Term,O,' is ',AV]),nl.

conjterm(wait-time,' for ').
conjterm(use-time,' of ').
%------------------------------------------------------------

print_queues :-
    queue(Time,O,TW,N,Q),
    QL is TW // Time,
    write_list(['Average queue length of ',O,' is ',QL]),nl,
    fail.
print_queues.

print_counters :-
    counter(X,N),
    write_list(['Total number of ',X,' is ',N]),nl,
    fail.
print_counters.

%===============================================================
% SIMULATION CONTINUE
%===============================================================
sim_cont(StopTime) :-
    state((CT,ST,Rest)),simul_run((CT,StopTime,Rest)).
sim_cont(StopTime,snap) :-
    state((CT,ST,Messs,States,Hist,Statis,Snap)),
    simul_run((CT,StopTime,Messs,States,Hist,Statis,snap)).

%---------------------------------------------------------------
% SNAP-SHOTS REPORT
%---------------------------------------------------------------
snap_shot(nosnap,_,_,_,_,_) :- !.
snap_shot(snap,CurTime,Messs,States,Hist,Statistics) :-
    write('Time      : '),write(CurTime),nl,
    write('Messages  : '),write_line(Messs),
    write('States    : '),write_line(States),
    write('Histograms: '),write_line(Hist),
    write('Statistics: '),write_line(Statistics),
    (setof(q(T,O,TW,N,Q),queue(T,O,TW,N,Q),List),!;
     List = [' ']),
    write('Queues    : '),write_line(List),
    (setof(count(Name,K),counter(Name,K),CList),!;
     CList = [' ']),
    write('Counters  : '),write_line(CList),nl,
    get0(C),nl.

store(X) :-
    (retract(state(_)),!; true),
    asserta(state(X)).

reset :-
    abolish(state/1),abolish(queue/5).

%----------------------------------------------------------

write_line([]) :- nl,!.
write_line([X|T]) :-
    write(X),nl,write_rest(T).

write_rest([]) :- !.
write_rest([X|T]) :-
    write('            '),write(X),nl,
    write_rest(T).

write_list([]) :- !.
write_list([H|T]) :-
    write(H), write_list(T).

%---------------------------------------------------------
among(A,(A,As)).
among(X,(A,As)) :- among(X,As).
among((A,B),(A,As)) :- among(B,As).
among(A,(A)) :- A \= (B,C).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

insert_list([],L,L) :- !.
insert_list([X|T],L,L1) :-
    insert(X,L,L2),
    insert_list(T,L2,L1).

insert(X,[],[X]) :- !.
insert((A,B,C),[(X,Y,Z)|T],[(A,B,C),(X,Y,Z)|T]) :-
    (A,B) @< (X,Y),!.
insert(X,[Y|T],[Y|R]) :- insert(X,T,R).

max(X,Y,Y) :- X =< Y, !.
max(X,Y,X) :- X > Y.

customer isa customer.

%--------------------------------------------------------------
% STATISTICAL FUNCTIONS
%--------------------------------------------------------------

uniform(A,B,R) :-
    randomz(P),
    X is A + (B - A)*P,
    roundup(X,R).

roundup(X,R) :-
    Y is X + 1,
    float_text(Y,T,fixed(0)),
    int_text(S,T), R is S - 1.

expon(M,R) :-
    randomz(P),
    X is M*(-ln(P)),
    roundup(X,R).

normal(M,S,R) :-
    randomz(P),
    standard_normal(P,Z),
    X is M + S*Z, roundup(X,R).

standard_normal(P,X) :-
    P < 0.5,!, Q is 1.0 - P,
    standard_normal(Q,Y), X is -Y.
standard_normal(P,X) :-
    std_norm(Table), linear_interpol(Table,P,X).

linear_interpol(Table,P,X) :-
    append(A,[(Q,Y),(R,Z)|Rest],Table), Q =< P, P < R,!,
    X is Y + (Z - Y)*(P - Q)/(R - Q).

std_norm([(0.5,0.0),(0.6,0.2533),(0.7,0.5244),
          (0.75,0.6745),(0.8,0.8416),(0.85,1.0364),
          (0.9,1.2816),(0.95,1.6449),(0.975,1.9600),
          (0.99,2.3263),(0.995,2.5758),(0.9975,2.8071),
          (0.999,3.0902),(0.9995,3.2905),(0.9999,3.7191),
          (1.001,3.9)]).

randomz(X) :- X is random.
%============================================================
% SIMULATION MODEL
%============================================================



