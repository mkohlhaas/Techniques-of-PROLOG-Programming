% citizen(john, australia).
% citizen(X, australia).
citizen(X, australia) :- born(X, australia).
citizen(X, australia) :- child(X, Y), citizen(Y, australia).
born(peter, new_zealand).
born(mary, australia).
child(john, peter).
child(john, mary).

% like(john, snake).
% eat(sue, X).
food(apple).  			  % Apples are food.
food(oyster).  		  	  % Oysters are food.
food(Y) :- alive(Y), eat(_, Y).   % Anything anyone eats and still alive is food.
like(john, X) :- food(X).	  % John likes all kinds of food.
eat(tom, snake).  		  % Tom eats snakes and still alive.
eat(sue, X) :- eat(tom, X).  	  % Sue eats everything Tom eats.
alive(snake). 			  % snakes are still alive.

% mother(X, Y). 
% mother_in_law(X, ann).
mother_in_law(X, Z) :- mother(X, Y), husband(Y,Z).
husband(john, ann).
mother(katy, john).
mother(lucy,katy).

% admires(X, father(X)) = admires(father(U), V).
% corner(X+Y) = corner(wall_st+perth_ave).
% X + l - Y * 2 = U - (1 + 2) * Z.
% 1 + 0 = 1.

% X = 2, Y is X+1.
% X = 2, Y = X+1.
% X = 2, Y == X+1.
% X = 2, Y =:= X+1.
% X = 2, 3 =:= X+1.
