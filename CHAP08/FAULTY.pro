fault(X) :- non(respond(X,Y)), X \== Y.
respond(a,a).
respond(a,b).
respond(a,c).
respond(a,d).
respond(b,a).
respond(b,b).
respond(c,X).

