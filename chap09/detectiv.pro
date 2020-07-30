p1(A:B,C:D) # 0.9 :- p2(A:B,C:D), p3(A:B,C:D).

p2(A:B,C:D) # 0.8 :- p4(A:B,C:D).
p2(A:B,C:D) # 0.95 :- p5(A:B,C:D).
p2(A:B,C:D) # 0.6 :- p6(A:B,C:D).
p3(A:B,C:D) # 0.9 :- p7(A:B,C:D),
    all(non(p8(A:B,E:F)),not(p8(A:B,E:G))).

p4(suspect:'Alan Smith',victim:'J.R.') # 1.
p5(suspect:'Sue R.',victim:'J.R.') # 1.
p6(suspect:'Sue R.',victim:'W.K.') # 1.
p7(suspect:'Sue R.',victim:'J.R.') # 0.9.
p7(suspect:'Alan Smith',victim:'J.R.') # 0.7.
p8(suspect:'Alan Smith',alibi:'John Cooper') # 1.
p8(suspect:'Alan Smith',alibi:'Peter Falk') # 1.

predicate([(suspect,A:B),murdered,(victim,C:D)],p1(A:B,C:D)).
predicate([(suspect,A:B),had,a,motive,to,kill,(victim,C:D)],
    p2(A:B,C:D)).
predicate([(suspect,A:B),had,the,potential,to,kill,
    (victim,C:D)],p3(A:B,C:D)).
predicate([(suspect,A:B),had,a,serious,conflict,with,
    (victim,C:D)],p4(A:B,C:D)).
predicate([(suspect,A:B),is,the,'life-insurance',beneficiary,
    of,(victim,C:D)],p5(A:B,C:D)).
predicate([(suspect,A:B),had,a,secret,known,to,(victim,C:D)],
    p6(A:B,C:D)).
predicate([(suspect,A:B),might,appear,near,(victim,C:D)],
    p7(A:B,C:D)).
predicate([(suspect,A:B),has,(alibi,C:D)],p8(A:B,C:D)).

domain(p1(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p2(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p3(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p4(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p5(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p6(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p7(A:_,C:_)) :- type(A,suspect),type(C,victim).
domain(p8(A:_,C:_)) :- type(A,suspect),type(C,alibi).

constsymbol(suspect:'Alan Smith').
constsymbol(victim:'J.R.').
constsymbol(suspect:'Sue R.').
constsymbol(victim:'W.K.').
constsymbol(alibi:'John Cooper').
constsymbol(alibi:'Peter Falk').
