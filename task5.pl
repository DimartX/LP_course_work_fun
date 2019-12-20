
ask_question(X, L) :-
    question(X, L, []),


question(question(X)) --> subject_q(X).
question(question(X)) --> general_q(X).
question(question(X)) --> special_q(X).    

question_word(question_word([X]), subj) --> [X], {member(X,[who,whose])}.
question_word(question_word([what]), spec) --> [what].
question_word(question_word([how,many]), spec) --> [how, many]. 
question_word(question_word([is, were, was]), gen) --> [is].

subject_q(subject_q(W1,W2)) -->
    question_word(W1, subj),
    addition(W2,W1,subj).

addition(W2, question_word([who]), QT) -->
    aux_verb(QT),
    article,
    person(P, posessive),
    rec_relative(W1,s),
    {W1 = relative(noperson, List),
     W2 = relative(P, List)}.
addition(W1, question_word([who]), QT) -->
    aux_verb(QT),
    article,
    rec_relative(W1,s),
    {W1 \= relative(noperson,_)}.
addition(W2, question_word([whose]), QT) -->
    rec_relative(W1,s),
    aux_verb(QT),
    person(P, common),
    {W1 = relative(noperson, List),
     W2 = relative(P, List)}. 

special_q(special_q(W1,W2)) -->
    question_word(W1,spec),
    {W1 = question_word([how,many])},
    rec_relative(WT2,pl),
    aux_verb(spec),
    man(X,common),
    verb,
    {WT2 = relative(noman, List),
     W2 = relative(X, List)}. 
special_q(special_q(W1,W)) -->
    question_word(W1,spec),
    {W1 = question_word([what])},
    after_question,
    man(X,common),
    [and],
    man(Y,common),
    {W = relative(X,Y)}. 

general_q(general_q(W2,W3)) -->
    aux_verb(gen),
    man(W2,common),
    rec_relative(W3,s).
general_q(general_q(W2,W3)) -->
    aux_verb(gen),
    man(W2,common),
    man(WT,posessive),
    rec_relative(WT2,s),
    {WT2 = relative(noman, List),
     W3 = relative(WT,List)}.


relative_word(T, s) -->
    [T],
    {member(T, [wife, husband, mother, father, brother, sister, son, daughter])}.
relative_word(R,pl) -->
    [X],
    {atom_chars(X,C),
     append(RT,[s],C),
     atom_chars(R,RT),
     relative_word(R,s,[R],[])}.

aux_verb(X) -->  [is], {member(X, [gen, subj])}.
aux_verb(X) -->  [do, did, does], {member(X, [spec])}.

verb() --> [X], {member(X, [have, has, had])}.

after_question --> synonym(kind), [of], synonym(relation), synonym(between).
synonym(A, B) :-
    member(X,[[kind,type],[relation,relations, relationship], [among, between]]),
    member(A,X),
    member(B,X).
synonym(A) --> [B], {synonym(A,B)}.

rec_relative(relative(P, L), Pl) -->
    relative_word(X, Pl),
    [of],
    rec_relative(relative(P,L1), Pl),
    {append([X], L1, L)}.
rec_relative(relative(P,[]), _) --> man(P, common).
rec_relative(relative(noman,[X]), Pl) --> relative_word(X,Pl).

man(X) :- male(X); female(X).
man(man(P), common) --> [P], {man(P)}.
man(man(P), common) --> [P], {member(P,[he,she,her,his])}.
man(man(X), posessive) --> [P], {atom_string(P,S), string_concat(S1,"\'s", S), atom_string(X,S1),man(X)}. %'
man(man(P), posessive) --> [P], {member(P,[his,her])}.


relative([X|T],Y,Z) :-
    !,
    dpth([Y],Z,_,A),
    [X|T] = A.

relative(X,Y,Z) :-
    dpth([Y],Z,_,A),
    [X] = A.

:- ['task4.pl']

