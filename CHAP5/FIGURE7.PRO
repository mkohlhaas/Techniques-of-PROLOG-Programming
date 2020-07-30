% FIGURE 5.7: An airport control system
%----------------------------------------------------------
% consult this program and enter the following query:
%   ?- airport_control.
% then enter messages in the following format (each message
% will have a response from the control tower):
%   AF24 request landing.
%   PN06 request taking-off.
%   QT15 request taking-off.
%   RF27 request taking-off.
%   HK20 request landing.
%   AF24 landing completed.
%   HK20 landing completed.
%   PN06 taking-off completed.
%   AI35 request landing.
%   QT15 taking-off completed.
%   AI35 landing completed.
%   RF27 taking-off completed.
%   zzzz.
%----------------------------------------------------------
:- op(500,xfy,:).

    airport_control :-
        repeat,
            receive_message(M),
            process_message(M),
            process_runway,
        terminate(M).

    receive_message(M) :-
        read_sentence(M),nl.

    process_message(Message) :-
        check_message(Message,Response),
        respond_message(Response).

    check_message(Message,Response) :-
        request_service(Message,Airplane,Response),!,
        add_to_queue(Airplane).
    check_message([Flight|Rest],Response) :-
        complete_service([Flight|Rest],_,Response),!,
        change_runway(busy:Flight,free:_).
    check_message(_,Response) :- error_message(Response).

    request_service([Flight,request,Service],Airplane,
        [Flight,Service,'request received. Please wait!']) :-
        valid(Flight,Service,Airplane).

    complete_service([Flight,Service,completed],_,
        [Flight,Service,'completion recorded.']) :-
        valid(Flight,Service,_).

    error_message(['Invalid message! Use following format:',nl,
        'FlightNumber request <landing/taking-off>',nl,
        'FlightNumber <landing/taking-off> completed',nl]).

    valid(Flight,landing,landing(Flight)).
    valid(Flight,'taking-off',takingoff(Flight)).

    add_to_queue(Airplane) :- assertz(Airplane).

    change_runway(A,B) :-
        retract(runway(A)),!,asserta(runway(B)).
    change_runway(_,_) :-
        respond_message(['Invalid flight. Try again.']),
        fail.

    process_runway :-
        runway(free:_),
        provide_service(Flight),
        change_runway(_,busy:Flight).
    process_runway.

    provide_service(Flight) :-
        retract(landing(Flight)),!,
        respond_message([Flight,'may proceed landing now.']).
    provide_service(Flight) :-
        retract(takingoff(Flight)),!,
        respond_message([Flight,'may proceed taking off now.']).
    provide_service(_) :-
        respond_message(['> Report: No airplane in sight.']),
        nl,fail.

    respond_message([]) :- nl,nl.
    respond_message([H|T]) :-
        (H = nl, nl,!; write(H),write(' ')),
        respond_message(T).

    runway(free:_).
    terminate([zzzz]).

% Read a sentence terminated by a period (Figure 4.9)
%-----------------------------------------------------
    read_sentence(L) :- read_sent(32,L).

    read_sent(C,L) :- end_sent(C),!,L = [].
    read_sent(C,[W|L]) :-
        read_word(W,C1),read_sent(C1,L).

    read_word(W,C1) :-
        read_first_char(S,C),
        read_word_chars(S,C,[],L,C1),
        reverse(L,[],L1),name(W,L1).

    read_first_char(S,C) :-
        get_char(S),start_word(S,C),!;
        read_first_char(S,C).

    read_word_chars(S,C,L,L,C1) :- end_word(S,C,C1),!.
    read_word_chars(S,8,[C|L],L1,C1) :- !,
        put(32),put(8),
        get_char(C2),
        read_word_chars(S,C2,L,L1,C1).
    read_word_chars(S,C,L,L1,C1) :-
        legal_char(S,C),
        get_char(C2),
        read_word_chars(S,C2,[C|L],L1,C1).

    get_char(C) :- get0(C),(C = 13,!,nl; true).
    valid_char(C) :-
        96 < C, C < 123;  % a-z
        64 < C, C < 91;   % A-Z
        47 < C, C < 58;   % 0-9
        C = 45.        % hyphen.

    start_word(C,C)   :- valid_char(C),!.
    start_word(39,C)  :- get_char(C).
    end_word(39,39,C) :- get_char(C).  % quotation marks
    end_word(Q,C,C)   :- Q \= 39,C \= 8,not(valid_char(C)).
    end_sent(C)       :- (C = 46; C = 63). % period or ?

    legal_char(39,_) :- !.
    legal_char(_,C)  :- valid_char(C).

    reverse([],L,L).
    reverse([H|T],L,L1) :- reverse(T,[H|L],L1).

%-----------------------------------------------------------



