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

what_relation(X, Y, brother) :-
    Y \= X,
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    male(X).

what_relation(X, Y, sister) :-
    Y \= X, 
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    female(X).

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

what_relation(X, Y, grandma) :-
    Y \= X,
    mother(Y, L),
    (father(L, X); mother(L, X)).

what_relation(X, Y, grandpa) :-
    Y \= X,
    father(Y, L),
    (father(L, X); mother(L, X)).

what_relation(X, Y, grandson) :-
    Y \= X,
    (father(X, L); mother(X, L)),    
    (father(L, Y); mother(L, Y)),
    male(Y).

what_relation(X, Y, granddaughter) :-
    Y \= X,
    (father(X, L); mother(X, L)),    
    (father(L, Y); mother(L, Y)),
    female(Y).


%

prolong_d([X|T], [Y,X|T], S) :-
    (male(X) ; female(X)),
    (male(Y) ; female(Y)),
    X \= Y,
    what_relation(X, Y, S),
    not(member(Y, [X|T])).

relative_d(A, X, Y) :-
    (male(X) ; female(X)),
    (male(Y) ; female(Y)),
    X \= Y,
    integer(N),
    dpth([X],Y,_,A, N).

dpth([X|T], X, [X|T], [], 0).
dpth(P, F, L, A, N) :-
    N > 0,
    prolong_d(P, P1, S),
    N1 is N - 1,
    dpth(P1, F, L, NA, N1),
    append(NA,[S], A).


% Поиск в ширину

chain_to_relations([], []).
chain_to_relations([_], []).
chain_to_relations([C1,C2|T], [R|N]) :-
    what_relation(C2, C1, R),
    chain_to_relations([C2|T], N).

relative_b(A, X, Y) :-
    (male(X) ; female(X)),
    (male(Y) ; female(Y)),
    X \= Y,
    bdth([[X]], Y, L),
    chain_to_relations(L, A).

prolong_b([X|T], [Y,X|T]) :-
    (male(X) ; female(X)),
    (male(Y) ; female(Y)),
    X \= Y,
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

?- relative_d([father], N, 'Martin Smith').
%@ ERROR: Undefined procedure: relative_d/3 (DWIM could not correct goal).
%@ N = 'Emil Smith' ;
%@ N = 'Emil Smith' ;
%@ N = 'Gustaf Smith Sr.' ;
%@ N = 'Gustaf Smith Sr.' ;
%@ N = 'Ingeman Smith' ;
%@ N = 'Ingeman Smith' ;
%@ N = 'Ingeman Smith' ;
%@ N = 'Ingeman Smith' ;
%@ N = 'Magnes Smith' ;
%@ N = 'Magnes Smith' ;
%@ N = 'Hanna Smith' ;
%@ N = 'Hanna Smith' ;
%@ N = 'Ingar Smith' ;
%@ N = 'Ingar Smith' ;
%@ false.

?- father(N, 'Martin Smith').
%@ N = 'Ingeman Smith' ;
%@ N = 'Martin Smith'.

?- what_relation('Martin Smith', N, father).
%@ false.
%@ false.
%@ false.

?- female(N), what_relation('Hanna Smith', N, father).


'Carl Emil Smith'

?- relative_d([brother, father], N, 'Martin Smith').

%@ false.
