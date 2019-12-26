:- ['Smith.pl'].

:- ['task4.pl'].
:- nb_setval(prev_female, noperson).
:- nb_setval(prev_male, noperson).

ask_question(R) :-
    read_string_to_list(R, A),
    question(Model, A, []),
    parse_model(Model, Args),
    handle_args(Args, Ans),
    print_res(Ans), !.


%?- ask_question("Who is Misha's brother?").
%@ The brother of Misha is Andrew
%@ true.
%?- ask_question("Who is brother of Misha?").
%@ The brother of Misha is Andrew
%@ true.
%?- ask_question("Whose brother is Misha?").
%@ Misha is brother of Andrew
%@ true.
%?- ask_question("Is Misha Andrew's brother?").
%@ Yes, Misha is brother of Andrew
%@ true.
%?- ask_question("What kind relations between Misha and Andrew?").
%@ Misha is brother of Andrew
%@ true.
%?- ask_question("Who is Misha for Andrew?").
%@ Misha is brother of Andrew
%@ true.
%?- ask_question("How many brothers does Misha have?").
%@ Misha have 1 brother
%@ true.

read_string_to_list(S, L) :-
    split_string(S, " ", " ?", L).

%?- read_string_to_list("Who  is Misha's brother?", L).
%@ L = ["Who", "is", "Misha's", "brother"].

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

%?- n1r_q(R, ["Who", "is", "Misha's", "brother"], []).
%@ R = n1r_q(question_word(["who"]), person("Misha"), relative(["brother"])) ;
%@ false.

n1r_q(n1r_q(W1, W3, W2)) -->
    question_word(W1, n1r_q),
    add_verb(be_forms),
    article,
    rec_relative(W2, _),
    ["of"],
    person(W3, common, []).

%?- n1r_q(R, ["Who", "is", "the", "brother", "of", "Misha"], []).
%@ R = n1r_q(question_word(["who"]), person("Misha"), relative(["brother"])) ;
%@ false.

n2r_q(n2r_q(W1, W2, W3)) -->
    question_word(W1, n2r_q),
    rec_relative(W3, _),
    add_verb(be_forms),
    person(W2, common, []).

%?- n2r_q(R, ["Whose", "brother", "is", "Misha"], []).
%@ R = n2r_q(question_word(["whose"]), person("Misha"), relative(["brother"])) ;
%@ false.

n1n2r_q(n1n2r_q(W1, W2, W3, W4)) -->
    question_word(W1, n1n2r_q),
    person(W2, common, []),
    person(W3, posessive, []),
    rec_relative(W4, s).

%?- n1n2r_q(R, ["Is", "Misha", "Andrew's", "brother"], []).
%@ R = n1n2r_q(question_word(["is"]), person("Misha"), person("Andrew"), relative(["brother"])) ;
%@ false.

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q1),
    [X], {member(X, ["kind", "type"])},
    ["relations", "between"],
    person(W2, common, []),
    ["and"],
    person(W3, common, []).

%?- rn1n2_q(R, ["What", "kind", "relations", "between", "Misha", "and", "Andrew"], []).
%@ R = rn1n2_q(question_word(["what"]), person("Misha"), person("Andrew")) ;
%@ false.

rn1n2_q(rn1n2_q(W1, W2, W3)) -->
    question_word(W1, rn1n2_q2),
    add_verb(be_forms),
    person(W2, common, []),
    ["for"],
    person(W3, common, []).

%?- rn1n2_q(R, ["Who", "is", "Misha", "for", "Andrew"], []).
%@ R = rn1n2_q(question_word(["who"]), person("Misha"), person("Andrew")) ;
%@ false.

num_q(num_q(W1, W2, W3)) -->
    question_word(W1, num_q),
    rec_relative(W2, _),
    add_verb(do_forms),
    person(W3, common, []),
    [X1], {string_lower(X1, X), member(X, ["have", "had"])}.

%?- num_q(R, ["how", "many", "brothers", "does", "Misha", "have"], []).
%@ R = num_q(question_word(["how", "many"]), relative(["brother"]), person("Misha")) ;
%@ false.

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

%?- rec_relative(R, _, ["daughter", "of", "sister"], []).
%@ R = relative(["daughter", "sister"]) ;
%@ false.

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

all_pers(R1, R2, _, []) --> person(R1, _, []), person(R2, _, []).

%?- all_pers(R1, R2, _, [], ["God's", "wife", "he"], []).
%@ R1 = person("God's wife"),
%@ R2 = person("he") ;
%@ R1 = person("God's wife"),
%@ R2 = person("he") ;
%@ false.
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ false.
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ R1 = person("God's wife"),
%@ R2 = person("Misha") ;
%@ false.


%?- person(R, _, ["Misha"], []).
%@ R = person("Misha") ;
%@ false.

add_verb(be_forms) --> [P1], {string_lower(P1, P), member(P, ["is", "was", "were"])}.
add_verb(do_forms) --> [P1], {string_lower(P1, P), member(P, ["do", "does", "did"])}.

article --> [X1], {string_lower(X1, X), member(X, ["the", "a", "an"])}.
article --> [].

parse_model(Model, model(n1r_q, P1, R)) :-
    Model = n1r_q(question_word(_), person(N1), relative(R)),
    handle_pronoun(N1, P1).

%?- parse_model(n1r_q(question_word(["who"]), person("Misha"), relative(["brother"])), Ans).
%@ Ans = model(n1r_q, "Misha", ["brother"]) ;
%@ false.

parse_model(Model, model(n2r_q, P2, R)) :-
    Model = n2r_q(question_word(_), person(N2), relative(R)),
    handle_pronoun(N2, P2).

%?- parse_model(n2r_q(question_word(["whose"]), person("Misha"), relative(["brother"])), Ans).
%@ Ans = model(n2r_q, "Misha", ["brother"]) ;
%@ false.

parse_model(Model, model(n1n2r_q, P1, P2, R)) :-
    Model = n1n2r_q(question_word(_), person(N1), person(N2), relative(R)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).

%?- parse_model(n1n2r_q(question_word(["is"]), person("Misha"), person("Andrew"), relative(["brother"])), Ans).
%@ Ans = model(n1n2r_q, "Misha", "Andrew", ["brother"]) ;
%@ false.

parse_model(Model, model(rn1n2_q, P1, P2)) :-
    Model = rn1n2_q(question_word(_), person(N2), person(N1)),
    handle_pronoun(N1, P1),
    handle_pronoun(N2, P2).    

%?- parse_model(rn1n2_q(question_word(["what"]), person("Misha"), person("Andrew")), Ans).
%@ Ans = model(rn1n2_q, "Andrew", "Misha") ;
%@ false.

parse_model(Model, model(num_q, P1, R)) :-
    Model = num_q(question_word(_), relative(R), person(N1)),
    handle_pronoun(N1, P1).

%?- parse_model(num_q(question_word(["how", "many"]), relative(["brother"]), person("Misha")), Ans).
%@ Ans = model(num_q, "Misha", ["brother"]) ;
%@ false.

handle_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    male(P), nb_setval(prev_male, P).

handle_pronoun(P, P) :-
    not(member(P, ["he", "his", "him", "she", "her"])),
    female(P), nb_setval(prev_female, P).

handle_pronoun(P, N) :-
    member(P, ["he", "his", "him"]),
    nb_getval(prev_male, N).

handle_pronoun(P, N) :-
    member(P, ["she", "her"]),
    nb_getval(prev_female, N).

handle_args(model(n1r_q, N1, R), Ans) :- 
    relative_d(R, N1, N2),
    add_of(R, R1),
    append(["The"], R1, First_part),
    append(First_part, ["of", N1, "is", N2], Ans).

%?- handle_args(model(n1r_q, "Misha", ["brother"]), Ans).
%@ Ans = ["The", "brother", "of", "Misha", "is", "Andrew"].
%@ false.

handle_args(model(n2r_q, N2, R), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).

%?- handle_args(model(n2r_q, "Misha", ["brother"]), Ans).
%@ Ans = ["Misha", "is", "brother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "brother", "of", "Andrew"] ;
%@ false.

handle_args(model(n1n2r_q, N2, N1, R), Ans) :-
    relative_d(R, N2, N1),
    add_of(R, R1),
    append(["Yes,", N2, "is"], R1, First_part),
    append(First_part, ["of", N1], Ans).

%?- handle_args(model(n1n2r_q, "Misha", "Andrew", ["brother"]), Ans).
%@ Ans = ["Yes,", "Misha", "is", "brother", "of", "Andrew"] ;
%@ Ans = ["Yes,", "Misha", "is", "brother", "of", "Andrew"] ;
%@ false.

handle_args(model(n1n2r_q, N2, N1, R), Ans) :-
    not(relative_d(R, N2, N1)),
    add_of(R, R1),
    append(["No,", N2, "is", "not"], R1, First_part),
    append(First_part, ["of", N1], Ans).

%?- handle_args(model(n1n2r_q, "Misha", "Sasha", ["brother"]), Ans).
%@ Ans = ["No,", "Misha", "is", "not", "brother", "of", "Sasha"] ;
%@ false.

handle_args(model(rn1n2_q, N2, N1), Ans) :-
    relative_d(R, N1, N2),
    add_of(R, R1),
    append([N1, "is"], R1, First_part),
    append(First_part, ["of", N2], Ans).

%?- handle_args(model(rn1n2_q, "Andrew", "Misha"), Ans).
%@ Ans = ["Misha", "is", "brother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "brother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "mother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "father", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "husband", "of", "mother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "husband", "of", "mother", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "wife", "of", "father", "of", "Andrew"] ;
%@ Ans = ["Misha", "is", "son", "of", "wife", "of", "father", "of", "Andrew"] ;
%@ false.

handle_args(model(rn1n2_q, N2, N1), Ans) :-
    not(relative_d(_, N1, N2)),
    Ans = ["there is no data to define relations"].

%?- handle_args(model(rn1n2_q, "Sasha", "Misha"), Ans).
%@ Ans = ["there is no data to define relations"].

handle_args(model(num_q, N1, R), Ans) :-
    setof(N2, relative_d(R, N1, N2), L),
    length(L, C),
    to_plural(R, R1, C),
    add_of(R1, R2),
    append([N1, "have", C], R2, Ans).

%?- handle_args(model(num_q, "Misha", ["brother"]), Ans).
%@ Ans = ["Misha", "have", 1, "brothers"] ;
%@ false.

add_of([X], [X]).
add_of([X|T], [X, "of"|T1]) :-
    add_of(T, T1).

%?- add_of(["1","2","3","4","5"], L).
%@ L = ["1", "of", "2", "of", "3", "of", "4", "of", "5"] ;
%@ false.

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

%?- to_plural(["brother", "father"], R).
%@ R = ["brothers", "father"] ;
%@ false.

print_res(L) :-
    atomics_to_string(L, ' ', S),
    write(S), nl, !.

%?- print_res(["The", "brother", "of", "Misha", "is", "Andrew"]).
%@ The brother of Misha is Andrew
%@ true.
