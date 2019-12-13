% Поиск Шурина (брата жены)

%shurin
shurin(Husb, Bro) :- wife(Husb, Wife), brother(Wife, Bro).

% wife

wife(X, T) :- father(X, L), mother(T, L), !.

brother(X, Z) :- (father(Y, X), father(Y, Z); mother(T, X), mother(T, Z)), male(Z), X \= Z.

?- ['Smith.pl'].
%@ true.

?- wife('Martin Smith', L).
%@ L = 'Kerstina Hansdotter'.

?- shurin('Edwin Michael Smith', T).
%@ T = 'Bill Adams' ;
%@ T = 'Greg Adams' ;
%@ T = 'Bill Adams' ;
%@ T = 'Greg Adams' ;
%@ false.

?- shurin(X, Y).
%@ X = 'Edwin Michael Smith',
%@ Y = 'Bill Adams' ;
%@ X = 'Edwin Michael Smith',
%@ Y = 'Greg Adams' ;
%@ X = 'Edwin Michael Smith',
%@ Y = 'Bill Adams' ;
%@ X = 'Edwin Michael Smith',
%@ Y = 'Greg Adams' ;
%@ false.
