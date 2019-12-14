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

% Добавлять ли то, что выше? 

what_relation(X, Y, brother) :-
    Y \= X,
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    male(Y).

what_relation(X, Y, sister) :-
    Y \= X, 
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    female(Y).

what_relation(Y, X, mother) :-
    Y \= X, 
    mother(X, Y).

what_relation(Y, X, father) :-
    Y \= X,
    father(X, Y).

what_relation(X, Y, husband) :-
    Y \= X,
    mother(X, L),
    father(Y, L).

what_relation(X, Y, wife) :-
    Y \= X,
    father(X, L),
    mother(Y, L).

what_relation(X, Y, son) :-
    Y \= X,
    (father(X, Y); mother(X, Y)),
    male(Y).

what_relation(X, Y, daughter) :-
    Y \= X,
    (father(X, Y); mother(X, Y)),
    female(Y).

% Поиск в глубину

prolong_d([X|T], [Y,X|T], S) :-
    what_relation(X, Y, S),
    not(member(Y, [X|T])).

relative_d(A,X,Y) :-
    dpth([X],Y,_,A).

dpth([X|T], X, [X|T], []).
dpth(P, F, L, A) :-
    prolong_d(P, P1, S),
    dpth(P1, F, L, NA),
    append(NA,[S], A).

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
?- ['Smith.pl'].

?- relative_d(N, 'Carl Emil Smith', 'Martin Smith').

%@ N = [brother, brother, brother, brother, husband, mother, sister, sister, brother|...] .

:-trace.
%@ true.

?- relative_b(N, 'Carl Emil Smith', 'Martin Smith').

%@ N = [brother, father] .
