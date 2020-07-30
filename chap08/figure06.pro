% FIGURE 8.6: Generation of new symbols
% ---------------------------------------------------
% consult this program and try the following queries:
%   ?- generate_symbol(Symb1).
%   ?- generate_symbol(Symb2).
%   ?- generate_symbol(Symb3).
% ---------------------------------------------------
generate_symbol(Symb) :-
    common_symbol(P),
    retract(symbol_index(I)),I1 is I+1,
    int_to_chars(I1,[],L),
    name(Symb,[P|L]),
    assert(symbol_index(I1)).

int_to_chars(0,L,L) :- !.
int_to_chars(I,L,L1) :-
    C is 48 + (I mod 10),
    I1 is I//10,
    int_to_chars(I1,[C|L],L1).

common_symbol(112).
symbol_index(24).
