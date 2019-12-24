% 4. Реализовать программу на языке Prolog, которая позволит определять степень родства двух произвольных индивидуумов в дереве. 

%% ?- relative(brother, ‘Петя’, X). 
%% X = ‘Вася’ 
%% ?- relative(W,’Петя’,’Вася’). 
%% X = brother 
%% X = child – father     
%% % т.е. Вася является ребенком отца Пети 
%% X = child – mother  
%% % т.е. Вася является ребенком матери Пети 

%% ?- bdth([['Hans Peter Smith']], 'Craig Peter Smith', P).
%% %@ P = ['Craig Peter Smith', 'Lloyd Smith', 'Hans Peter Smith'].
%% %@ P = ['Craig Peter Smith', 'Lloyd Smith', 'Hans Peter Smith'].

%  Для X-а Y является

what_relation(Y, X, brother) :-
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    male(X),
    Y \= X.

what_relation(Y, X, sister) :-
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    female(X),
    Y \= X.

what_relation(Y, X, mother) :-
    mother(X, Y),
    Y \= X.

what_relation(Y, X, father) :-
    father(X, Y),
    Y \= X.

what_relation(X, Y, husband) :-
    mother(X, L),
    father(Y, L),
    Y \= X.

what_relation(X, Y, wife) :-
    father(X, L),
    mother(Y, L),
    Y \= X.

what_relation(X, Y, son) :-
    (father(X, Y); mother(X, Y)),
    male(Y),
    Y \= X.

what_relation(X, Y, daughter) :-
    (father(X, Y); mother(X, Y)),
    female(Y),
    Y \= X.

% ID-search

prolong_d([X|T], [Y1|T1], [Y,X|T], T1) :-
    what_relation(X, Y, Y1),
    not(member(Y, [X|T])).

relative_d(A, X, Y) :-
    between(1, 4, N),
    dpth([X], Y, _, L, N),
    reverse(L, A).

dpth([X|T], X, [X|T], [], 0).
dpth(P, F, L, A, N) :-
    N > 0,
    prolong_d(P, A, P1, A1),
    N1 is N - 1,
    dpth(P1, F, L, A1, N1).


% Поиск в ширину

chain_to_relations([], []).
chain_to_relations([_], []).
chain_to_relations([C1,C2|T], [R|N]) :-
    what_relation(C2, C1, R),
    chain_to_relations([C2|T], N).

relative_b(A, X, Y) :-
    bdth([[X]], Y, L),
    chain_to_relations(L, A).

prolong_b([X|T], [Y,X|T]) :-
    what_relation(Y, X, _),
    not(member(Y, [X|T])).

bdth([[X|T]|_], X, [X|T]).
bdth([P|QI], X, R) :-
    findall(Z, prolong_b(P, Z), T),
    append(QI, T, QO),  
    bdth(QO, X, R), !.
bdth([_|T], Y, L) :-
    bdth(T, Y, L).

% Загрузка отношений
%?- ['Smith.pl'].

male('Misha').
male('Andrew').
male('Sasha').
father('God', 'Misha').
father('God', 'Andrew').
%?- relative_d(R, 'Andrew', 'Misha').
%@ R = [brother] ;
%@ R = [son, father] ;
%@ false.
%@ R = [brother] ;
%@ R = [son, father] ;
%@ false.
    
    
%?- relative_d([brother, father], N, 'Martin Smith').
%@ N = 'Astrid Shermanna Augusta Smith' ;
%@ N = 'Carl Emil Smith' ;
%@ N = 'Gus Smith' ;
%@ N = 'Hans Peter Smith' ;
%@ N = 'Hjalmar Smith' ;
%@ N = 'Hjalmar Smith' ;
%@ N = 'Kirsti Marie Smith' ;
%@ false.

%?- what_relation('Amber Marie Smith', 'Mason Michael Smith', N).
%@ N = brother ;
%@ N = brother ;
%@ false.
%@ N = sister ;
%@ N = sister ;
%@ false.
%@ false.
%@ false.
%@ false.

%?- relative_d([mother, father], 'Amber Marie Smith', B).
%@ B = 'Alice Paula Perkins' ;
%@ false.
%@ B = 'Janice Ann Adams' ;
%@ false.
%@ B = 'Mason Michael Smith' ;
%@ B = 'Mason Michael Smith' ;
%@ false.
%@ false.
