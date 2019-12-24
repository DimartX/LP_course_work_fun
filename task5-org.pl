male('Misha').
male('Andrew').
male('Sasha').
female('God\'s wife').
mother('God\'s wife', 'Misha').
mother('God\'s wife', 'Andrew').
father('God', 'Misha').
father('God', 'Andrew').

:- ['task4.pl'].
:- nb_setval(prev_female, noperson).
:- nb_setval(prev_male, noperson).

ask_question(A) :-
    question(Model, A, []),
    parse_model(Model, Args),
    handle_args(Args, Ans),
    print_res(Ans), !.

%?- ask_question([who, is, 'Misha\'s', brother]).
%@ the brother of Misha is Andrew
%@ true.
%?- ask_question([who, is, brother, of, 'Misha']).
%@ the brother of Misha is Andrew
%@ true.
%?- ask_question([whose, brother, is, 'Misha']).
%@ Misha is brother of Andrew
%@ true.
%?- ask_question([is, 'Misha', 'Andrew\'s', brother]).
%@ yes Andrew is brother of Misha
%@ true.
%?- ask_question([what, kind, relations, between, 'Misha', and, 'Andrew']).
%@ Andrew is brother of Misha
%@ true.
%?- ask_question([who, is, 'Misha', for, 'Andrew']).
%@ Andrew is brother of Misha
%@ true.
%?- ask_question([how, many, brothers, does, 'Misha', have]).
%@ Misha have 1 brothers
%@ true.

question(X) --> n1r_q(X). % Who is n1 R
question(X) --> n2r_q(X). % Whose R is n2  
question(X) --> rn1n2_q(X). % What kind R between n1 and n2
question(X) --> n1n2r_q(X). % Is/was n1 n2 R
question(X) --> num_q(X). % How many

n1r_q(n1r_q(W1, W2, W3)) -->
    question_word(W1, n1r_q),
    add_verb(be_forms),
    person(W2, posessive),
    rec_relative(W3, _).

%?- n1r_q(R, [Who, is, 'Misha\'s', brother], []).
%@ R = n1r_q(question_word([who]), person('Misha'), relative([brother])),
%@ Who = who ;
%@ false.

n1r_q(n1r_q(W1, W3, W2)) -->
    question_word(W1, n1r_q),
    add_verb(be_forms),
    article,
    rec_relative(W2, _),
    [of],
    person(W3, common).

%?- n1r_q(R, [Who, is, the, brother, of, Misha], []).
%@ R = n1r_q(question_word([who]), person('Misha'), relative([brother])),
%@ Who = who ;
%@ false.

n2r_q(n2r_q(W1, W2, W3)) -->
    question_word(W1, n2r_q),
    rec_relative(W3, _),
    add_verb(be_forms),
    person(W2, common).

%?- n2r_q(R, [Whose, brother, is, Misha], []).
%@ R = n2r_q(question_word([whose]), person('Misha'), relative([brother])),
%@ Whose = whose ;
%@ false.

n1n2r_q(n1n2r_q(W1, W2, W3, W4)) -->
    question_word(W1, n1n2r_q),
    person(W2, common),
    person(W3, posessive),
    rec_relative(W4, s).

%?- n1n2r_q(R, [is, 'Misha', 'Andrew\'s', brother], []).
%@ R = n1n2r_q(question_word([is]), person('Misha'), person('Andrew'), relative([brother])) ;
%@ false.

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q1),
    [X], {member(X, [kind, type])},
    [relations], [between],
    person(W2, common),
    [and],
    person(W3, common).

%?- rn1n2_q(R, [what, kind, relations, between, 'Misha', and, 'Andrew'], []).
%@ R = rn1n2_q(question_word([what]), person('Misha'), person('Andrew')) ;
%@ false.

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q2),
    add_verb(be_forms),
    person(W2, common),
    [for],
    person(W3, common).

%?- rn1n2_q(R, [who, is, 'Misha', for, 'Andrew'], []).
%@ R = rn1n2_q(question_word([who]), person('Misha'), person('Andrew')) ;
%@ false.

num_q(num_q(W1, W2, W3)) -->
    question_word(W1, num_q),
    rec_relative(W2, _),
    add_verb(do_forms),
    person(W3, common),
    [X], {member(X, [have, had])}.

%?- num_q(R, [how, many, brothers, does, 'Misha', have], []).
%@ R = num_q(question_word([how, many]), relative([brother]), person('Misha')) ;
%@ false.

question_word(question_word([who]), n1r_q) --> [who].
question_word(question_word([whose]), n2r_q) --> [whose].
question_word(question_word([what]), rn1n2_q1) --> [what].
question_word(question_word([who]), rn1n2_q2) --> [who].
question_word(question_word([X]), n1n2r_q) --> [X], {member(X, [is, was])}.
question_word(question_word([how,many]), num_q) --> [how, many].

rec_relative(relative(L), Pl) -->
    relative_noun(X, Pl),
    [of],
    rec_relative(relative(L1), Pl),
    {append([X], L1, L)}.
rec_relative(relative([X]), Pl) --> relative_noun(X, Pl).

relative_noun(X,s) --> [X], {member(X, [brother, sister, mother, child, father, wife, husband, son, daughter])}.
relative_noun(R,pl) --> [X], {atom_chars(X,C), append(RT,[s],C), atom_chars(R,RT), relative_noun(R,s,[R],[])}.

%?- rec_relative(R, _, [daughter, of, sister], []).
%@ R = relative([daughter, sister]) ;
%@ false.

person(X) :- male(X) ; female(X).
person(person(P), common) --> [P], {person(P)}.
person(person(P), common) --> [P], {member(P, [he, she])}.
person(person(X), posessive) --> [P], {atom_string(P,S), string_concat(S1,"\'s", S), atom_string(X,S1),person(X)}. %'
person(person(P), posessive) --> [P], {member(P,[his,her])}.

%?- person(R, _, ['Amber Marie Smith'], []).
%@ R = person('Amber Marie Smith') ;
%@ false.

add_verb(X) --> {member(X, [be_forms])}, [P], {member(P, [is, was, were])}.
add_verb(X) --> {member(X, [do_forms])}, [P], {member(P, [do, does, did])}.

article --> [X], {member(X, [the, a, an])}.
article --> [].

parse_model(Model, model(n1r_q, P1, R)) :-
    Model = n1r_q(question_word(_), person(N1), relative(R)),
    handle_pronoun(N1, P1).

%?- parse_model(n1r_q(question_word([who]), person('Misha'), relative([brother])), Ans).
%@ Ans = model(n1r_q, 'Misha', [brother]) ;
%@ false.

parse_model(Model, model(n2r_q, P2, R)) :-
    Model = n2r_q(question_word(_), person(N2), relative(R)),
    handle_pronoun(N2, P2).

%?- parse_model(n2r_q(question_word([whose]), person('Misha'), relative([brother])), Ans).
%@ Ans = model(n2r_q, 'Misha', [brother]) ;
%@ false.

parse_model(Model, model(n1n2r_q, P1, P2, R)) :-
    Model = n1n2r_q(question_word(_), person(N1), person(N2), relative(R)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).

%?- parse_model(n1n2r_q(question_word([is]), person('Misha'), person('Andrew'), relative([brother])), Ans).
%@ Ans = model(n1n2r_q, 'Misha', 'Andrew', [brother]) ;
%@ false.

parse_model(Model, model(rn1n2_q, P1, P2)) :-
    Model = rn1n2_q(question_word(_), person(N2), person(N1)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).    

%?- parse_model(nr1r2_q(question_word([what]), person('Misha'), person('Andrew')), Ans).
%@ Ans = model(rn1n2_q, 'Andrew', 'Misha') ;
%@ false.

parse_model(Model, model(num_q, P1, R)) :-
    Model = num_q(question_word(_), relative(R), person(N1)),
    handle_pronoun(N1, P1).

%?- parse_model(num_q(question_word([how, many]), relative([brother]), person('Misha')), Ans).
%@ Ans = model(num_q, 'Misha', [brother]) ;
%@ false.

handle_pronoun(P, P) :-
    not(member(P, [he, his, him, she, her])),
    male(P), nb_setval(prev_male, P).

handle_pronoun(P, P) :-
    not(member(P, [he, his, him, she, her])),
    female(P), nb_setval(prev_female, P).

handle_pronoun(P, N) :-
    member(P, [he, his, him]),
    nb_getval(prev_male, N).

handle_pronoun(P, N) :-
    member(P, [she, her]),
    nb_getval(prev_female, N).

handle_args(model(n1r_q, N1, R), Ans) :- 
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([the], R1, First_part),
    append(First_part, [of, N1, is, N2], Ans).

%?- handle_args(model(n1r_q, 'Misha', [brother]), Ans).
%@ Ans = [the, brother, of, 'Misha', is, 'Andrew'] ;
%@ false.

handle_args(model(n2r_q, N2, R), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N2, is], R1, First_part),
    append(First_part, [of, N1], Ans).

%?- handle_args(model(n2r_q, 'Misha', [brother]), Ans).
%@ Ans = ['Misha', is, brother, of, 'Andrew'] ;
%@ false.

handle_args(model(n1n2r_q, N1, N2, R), Ans) :-
    relative_d(R, N2, N1),
    add_of(R, R1),
    append([yes, N2, is], R1, First_part),
    append(First_part, [of, N1], Ans).

%?- handle_args(model(n1n2r_q, 'Misha', 'Andrew', [brother]), Ans).
%@ Ans = [yes, 'Andrew', is, brother, of, 'Misha'] ;
%@ false.

handle_args(model(n1n2r_q, N1, N2, R), Ans) :-
    not(relative_d(R, N2, N1)),
    add_of(R, R1),
    append([no, N2, is, not], R1, First_part),
    append(First_part, [of, N1], Ans).

%?- handle_args(model(n1n2r_q, 'Misha', 'Sasha', [brother]), Ans).
%@ Ans = [no, 'Sasha', is, not, brother, of, 'Misha'] ;
%@ false.

handle_args(model(rn1n2_q, N1, N2), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N1, is], R1, First_part),
    append(First_part, [of, N2], Ans).

%?- handle_args(model(rn1n2_q, 'Andrew', 'Misha'), Ans).
%@ Ans = ['Andrew', is, brother, of, 'Misha'] ;
%@ Ans = ['Andrew', is, brother, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, mother, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, father, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, husband, of, mother, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, husband, of, mother, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, wife, of, father, of, 'Misha'] ;
%@ Ans = ['Andrew', is, son, of, wife, of, father, of, 'Misha'] ;
%@ false.

handle_args(model(rn1n2_q, N1, N2), Ans) :-
    not(relative_d(_, N1, N2)),
    Ans = [there, is, no, data, to, define, relations].

%?- handle_args(model(rn1n2_q, 'Sasha', 'Misha'), Ans).
%@ Ans = [there, is, no, data, to, define, relations].

handle_args(model(num_q, N1, R), Ans) :-
    setof(N2, relative_d(R, N1, N2), L),
    length(L, C),
    to_plural(R, R1),
    add_of(R1, R2),
    append([N1, have, C], R2, Ans).

%?- handle_args(model(num_q, 'Misha', [brother]), Ans).
%@ Ans = ['Misha', have, 1, brothers] ;
%@ false.

add_of([X], [X]).
add_of([X|T], [X, of|T1]) :-
    add_of(T, T1).

%?- add_of([1,2,3,4,5], L).
%@ L = [1, of, 2, of, 3, of, 4, of, 5] ;
%@ false.

to_plural([], []).
to_plural([X|T], [X1|T1]) :-
    not(member(X, [father, mother, wife, husband])),
    term_string(X, Y),
    string_chars(Y, Z),
    append(Z, [s], Z1),
    string_chars(Y1, Z1),
    term_string(X1, Y1),
    to_plural(T, T1).
to_plural([X|T], [X|T1]) :-
    member(X, [father, mother, wife, husband]),
    to_plural(T, T1).

%?- to_plural([brother, father], R).
%@ R = [brothers, father] ;
%@ false.

print_res(L) :-
    atomics_to_string(L, ' ', S),
    write(S), nl, !.

%?- print_res([the, brother, of, 'Misha', is, 'Andrew']).
%@ the brother of Misha is Andrew
%@ true.
