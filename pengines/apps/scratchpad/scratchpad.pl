:- module(scratchpad,
	      [ ask_question/2,
            nb_linkval/2
	  ]).

:- scratchpad:nb_setval(prev_female, noperson).
:- scratchpad:nb_setval(prev_male, noperson).

ask_question(R, N) :-
    read_string_to_list(R, A),
    question(Model, A, []),
    parse_model(Model, Args),
    handle_args(Args, Ans),
    print_res(Ans, N), !.

read_string_to_list(S, L) :-
    split_string(S, " ", " ?", L).

question(X) --> n1r_q(X). % Who is n1 R
question(X) --> n2r_q(X). % Whose R is n2  
question(X) --> rn1n2_q(X). % What kind R between n1 and n2
question(X) --> n1n2r_q(X). % Is/was n1 n2 R
question(X) --> num_q(X). % How many

n1r_q(n1r_q(W1, W2, W3)) -->
    question_word(W1, n1r_q),
    add_verb(be_forms),
    person(W2, posessive, []),
    rec_relative(W3, _).

n1r_q(n1r_q(W1, W3, W2)) -->
    question_word(W1, n1r_q),
    add_verb(be_forms),
    article,
    rec_relative(W2, _),
    ["of"],
    person(W3, common, []).

n2r_q(n2r_q(W1, W2, W3)) -->
    question_word(W1, n2r_q),
    rec_relative(W3, _),
    add_verb(be_forms),
    person(W2, common, []).

n1n2r_q(n1n2r_q(W1, W2, W3, W4)) -->
    question_word(W1, n1n2r_q),
    person(W2, common, []),
    person(W3, posessive, []),
    rec_relative(W4, s).

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q1),
    [X], {member(X, ["kind", "type"])},
    ["relations", "between"],
    person(W2, common, []),
    ["and"],
    person(W3, common, []).

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q2),
    add_verb(be_forms),
    person(W2, common, []),
    ["for"],
    person(W3, common, []).

num_q(num_q(W1, W2, W3)) -->
    question_word(W1, num_q),
    rec_relative(W2, _),
    add_verb(do_forms),
    person(W3, common, []),
    [X1], {string_lower(X1, X), member(X, ["have", "had"])}.

question_word(question_word(["who"]), n1r_q) --> [X], {string_lower(X, "who")}.
question_word(question_word(["whose"]), n2r_q) --> [X], {string_lower(X, "whose")}.
question_word(question_word(["what"]), rn1n2_q1) --> [X], {string_lower(X, "what")}.
question_word(question_word(["who"]), rn1n2_q2) --> [X], {string_lower(X, "who")}.
question_word(question_word([X]), n1n2r_q) --> [X], {string_lower(X, Y), member(Y, ["is", "was"])}.
question_word(question_word(["how", "many"]), num_q) --> [X, Y], {string_lower(X, X1), string_lower(Y, Y1), X1 = "how", Y1 = "many"}.

rec_relative(relative(L), Pl) -->
    relative_noun(X, Pl),
    [Of], {string_lower(Of, "of")},
    rec_relative(relative(L1), Pl),
    {append([X], L1, L)}.
rec_relative(relative([X]), Pl) --> relative_noun(X, Pl).

relative_noun(Y,s) --> [X], {string_lower(X, Y), 
                             member(Y, ["brother", "sister", "mother", "child", "father", "wife", "husband", "son", "daughter"])}.
relative_noun(R,pl) --> [X], {string_concat(R,"s",X), relative_noun(R,s,[R],[])}.

person(person(X), common, X) :-
    male(X) ; female(X).

person(R, posessive, P) :-
    string_concat(X,"'s", P),
    person(R, common, X).

person(Pers, CP, L) --> [],
                        {reverse(L, R),
                         atomics_to_string(R, ' ', P),
                         person(Pers, CP, P)}, !.
person(Pers, CP, L) --> [P], person(Pers, CP, [P|L]), !.
person(Pers, CP, L) --> [P], {reverse([P|L], R),
                             atomics_to_string(R, ' ', P2),
                             person(Pers, CP, P2)}, !.

person(person(P), posessive, _) --> [P1], {string_lower(P1, P), member(P,["his","her","him"])}.

person(person(P), common, _) --> [P1], {string_lower(P1, P), member(P, ["he", "she"])}.

add_verb(be_forms) --> [P1], {string_lower(P1, P), member(P, ["is", "was", "were"])}.
add_verb(do_forms) --> [P1], {string_lower(P1, P), member(P, ["do", "does", "did"])}.

article --> [X1], {string_lower(X1, X), member(X, ["the", "a", "an"])}.
article --> [].

parse_model(Model, model(n1r_q, P1, R)) :-
    Model = n1r_q(question_word(_), person(N1), relative(R)),
    handle_pronoun(N1, P1).

parse_model(Model, model(n2r_q, P2, R)) :-
    Model = n2r_q(question_word(_), person(N2), relative(R)),
    handle_pronoun(N2, P2).

parse_model(Model, model(n1n2r_q, P1, P2, R)) :-
    Model = n1n2r_q(question_word(_), person(N1), person(N2), relative(R)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).

parse_model(Model, model(rn1n2_q, P1, P2)) :-
    Model = rn1n2_q(question_word(_), person(N2), person(N1)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).    

parse_model(Model, model(num_q, P1, R)) :-
    Model = num_q(question_word(_), relative(R), person(N1)),
    handle_pronoun(N1, P1).

handle_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    male(P), scratchpad:nb_setval(prev_male, P).

handle_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    female(P), scratchpad:nb_setval(prev_female, P).

handle_pronoun(P, N) :-
    member(P, ["he", "his", "him"]),
    scratchpad:nb_getval(prev_male, N).

handle_pronoun(P, N) :-
    member(P, ["she", "her"]),
    scratchpad:nb_getval(prev_female, N).

handle_args(model(n1r_q, N1, R), Ans) :- 
    relative_d(R, N1, N2),
    add_of(R, R1),
    append(["The"], R1, First_part),
    append(First_part, ["of", N1, "is", N2], Ans).

handle_args(model(n2r_q, N2, R), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).

handle_args(model(n1n2r_q, N2, N1, R), Ans) :-
    relative_d(R, N2, N1),
    add_of(R, R1),
    append(["Yes,", N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).

handle_args(model(n1n2r_q, N2, N1, R), Ans) :-
    not(relative_d(R, N2, N1)),
    add_of(R, R1),
    append(["No,", N2, "is", "not"], R1, First_part),
    append(First_part, ["of", N1], Ans).

handle_args(model(rn1n2_q, N2, N1), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N1, "is"], R1, First_part),
    append(First_part, ["of", N2], Ans).

handle_args(model(rn1n2_q, N2, N1), Ans) :-
    not(relative_d(_, N1, N2)),
    Ans = ["there is no data to define relations"].

handle_args(model(num_q, N1, R), Ans) :-
    setof(N2, relative_d(R, N1, N2), L),
    length(L, C),
    to_plural(R, R1, C),
    add_of(R1, R2),
    append([N1, "have", C], R2, Ans).

add_of([X], [X]).
add_of([X|T], [X, "of"|T1]) :-
    add_of(T, T1).

to_plural(R, R, 1).
to_plural(R, R1, _) :-
    to_plural(R, R1).

to_plural([], []).
to_plural([Y|T], [Y1|T1]) :-
    not(member(Y, ["father", "mother", "wife", "husband"])),
    string_chars(Y, Z),
    append(Z, [s], Z1),
    string_chars(Y1, Z1),
    to_plural(T, T1).
to_plural([X|T], [X|T1]) :-
    member(X, ["father", "mother", "wife", "husband"]),
    to_plural(T, T1).

print_res(L, S) :-
    atomics_to_string(L, ' ', S).


what_relation(Y, X, "brother") :-
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    male(X),
    Y \= X.

what_relation(Y, X, "sister") :-
    (father(T,X), father(T,Y); mother(T,X), mother(T,Y)),
    female(X),
    Y \= X.

what_relation(Y, X, "mother") :-
    mother(X, Y),
    Y \= X.

what_relation(Y, X, "father") :-
    father(X, Y),
    Y \= X.

what_relation(X, Y, "husband") :-
    mother(X, L),
    father(Y, L),
    Y \= X.

what_relation(X, Y, "wife") :-
    father(X, L),
    mother(Y, L),
    Y \= X.

what_relation(X, Y, "son") :-
    (father(X, Y); mother(X, Y)),
    male(Y),
    Y \= X.

what_relation(X, Y, "daughter") :-
    (father(X, Y); mother(X, Y)),
    female(Y),
    Y \= X.

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
