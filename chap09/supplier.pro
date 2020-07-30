p1(A:B) # 1 :- non(p2(A:B,C:D)).

p2(agent:'Adams & Sons',item:b1) # 1.
p2(agent:'Adams & Sons',item:b2) # 1.
p2(agent:'Adams & Sons',item:b3) # 1.
p2(agent:'Johnny Ltd',item:b1) # 1.
p2(agent:'Johnny Ltd',item:b2) # 1.
p2(agent:'Mitre 10',item:b1) # 1.

predicate([(agent,A:B),is,a,fractional,supplier],p1(A:B)).
predicate([(agent,A:B),supply,(item,C:D)],p2(A:B,C:D)).
predicate([(agent,A:B),not,supply,(item,C:D)],non(p2(A:B,C:D))).

domain(p1(A:_)) :- type(A,agent).
domain(p2(A:_,C:_)) :- type(A,agent),type(C,item).

constsymbol(agent:'Adams & Sons').
constsymbol(agent:'Johnny Ltd').
constsymbol(agent:'Mitre 10').
constsymbol(item:b1).
constsymbol(item:b2).
constsymbol(item:b3).
