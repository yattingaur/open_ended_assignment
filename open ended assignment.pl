do('chat,talk,think,write,read.').

task(chat,'I am currently chatting. :').
task(talk,'I am currently talking. :)').
task(think,'I am currently thinking. :)').
task(write,'See I have written this statement itself. :)').
task(read,'Yes I will do read your commands :)').

tellme(yoga,'Yoga is a set of specific exercises, called poses, combined with specific breathing techniques  :)').
tellme(balanceddiet,'A healthy diet is one that helps maintain or improve overall health.  :)').

howtodo(pushups,'1. Get down on all fours, placing your hands slightly wider than your shoulders.

2. Straighten your arms and legs.

3. Lower your body until your chest nearly touches the floor.

4. Pause, then push yourself back up.

5. Repeat.   :)').
howtodo(crunches,'To perform ab crunches correctly, you only have to lift your shoulders and head up off the ground just enough to feel your abs flexing.  :)').

suggestme(healthtip,'Base your meals on higher fibre starchy carbohydrates :)').

disease(diabetes,'walking, cycling, swimming, weightlifting......').
disease(asthma,'diaphragmatic breathing, nasal breathing, yoga breathing....').
disease(backpain,'partial crunches, wall sits, hamstring stretches....').
disease(cancer,'stretching, brisk walking, jogging, swimming....').
disease(arthritis,'pilates, water exercises, cycling, hand exercises....').
nodisease(healthy).
nodisease(fit).

positive(happy).
positive(nice).
positive(well).
positive(good).
positive(excited).
negative(bored).
negative(sad).
negative(unwell).
negative(stressed).
negative(depressed).

positivef(awesome).
positivef(amazing).
positivef(nice).
positivef(smart).
positivef(interesting).
negativef(boring).
negativef(irritating).
negativef(stupid).
negativef(frustating).

hypothesis(cold):-cold, !.
hypothesis(flu):-flu, !.
hypothesis(ebola):-ebola, !.
hypothesis(measles):-measles, !.
hypothesis(arthritis):-arthritis, !.
hypothesis(unknown).

/*Hypothesis Identification Rules */

cold:-
    verify(headache),
    verify(runny_nose),
    verify(sneezing),
    verify(sore_throat).

flu:-
    verify(fever),
    verify(headache),
    verify(chills),
    verify(body_ache).

ebola:-
    verify(headache),
    verify(rash),
    verify(nausea),
    verify(bleeding).

measles:-
    verify(fever),
    verify(runny_nose),
    verify(rash),
    verify(conjunctivitis).

arthritis:-
    verify(pain_in_joints),
    verify(stiffness_in_joints),
    verify(fatigue).

/*dynamic list for randomly selecting a response*/
:- dynamic confuse/1.
confuse(['Please speak again.','OK,thats something new.','Oh,I didnt know it.']).

start:-
    write('Hello, I am HeBot.'),nl,nl,
    eliza_loop.

eliza_loop:-
    write('You :-'),
    read(L),
    write('HeBot :- '),
    reply(L).

/*Can you questions*/
reply([can,you,X | _ ]):-
    (   task(X,Y)
    ->  write('yes I can '), write(X),nl,nl
    ,eliza_loop
    ).

/*intro reply*/
reply([hello,i,am,Name | _ ]):-
    write('Hello, '),write('! Its nice meeting you.'),nl,nl,
    eliza_loop.

/*reply*/
reply([suggest,me,a,X | _ ]):-
    (   suggestme(X,Y)
    ->  write(Y),nl,nl
    ,eliza_loop
    ;   write('Sorry, I cant suggest a '),write(X),nl,nl
    ,eliza_loop
    ).

reply([tell,me,about,X | _ ]):-
    (   tellme(X,Y)
    ->  write(Y),nl,nl
    ,eliza_loop
    ;   write('Sorry, I cant tell about '),write(X),nl,nl
    ,eliza_loop
    ).

reply([how,to,do,X | _ ]):-
    (   howtodo(X,Y)
    ->  write(Y),nl,nl
    ,eliza_loop
    ;   write('Sorry, I cant tell how to do '),write(X),nl,nl
    ,eliza_loop
    ).

/*perform tasks*/
reply([X | _]):-
    task(X,Y),write(Y),nl,nl,eliza_loop.

/*tell todays date*/
reply([tell,me,todays,date]):-
    date(X),write(X),nl,nl,
    eliza_loop.

/*eliza's activities*/
reply([what,can,you,do | _]):-
    write('I can '),do(X),write(X),nl,nl,
    eliza_loop.

/*How are you reply*/
reply([how,are,you]):-
    write('I am good, its nice talking to you.'),nl,
    write('How are you?'),nl,nl,
    eliza_loop.

/*reply to persons mood*/
reply([i,am,X | _]):-
    /*if positive mood*/
    (   positive(X) ->
    write('Thats great that you are '),write(X),nl,nl,eliza_loop
    /*if negative mood*/
    ;   (negative(X) ->
        write('Oh, please dont feel yourself'),nl,nl,eliza_loop
        /*else if didnt understood*/
        ;   write('Oh, I didnt knew it....'),nl,nl,eliza_loop
        )
    ).

reply([i,have,X | _]):-
    /*if positive mood*/
    (   disease(X,Y) ->
    write('Then you have to do exercises like:  '),write(Y),nl,nl,eliza_loop
    /*if negative mood*/
    ;   (nodisease(X) ->
        write('Thats great, keep yourself '),write(X),nl,nl,eliza_loop
        /*else if didnt understood*/
        ;   write('Oh, I didnt knew it....'),nl,nl,eliza_loop
        )
    ).

/*reply to feedback*/
reply([you,are,X | _ ]):-
    /*if positive feedback*/
    (   positivef(X) ->
    write('Thank you! You too are '),write(X),nl,nl,eliza_loop
    /*if negative feedback*/
    ;  ( negativef(X) ->
    write('I am sorry for being a '),write(X),write(' chatbot'),nl,nl,eliza_loop
    /*else if didnt understand feedback*/
    ;   write('Oh, I might be so...'),nl,nl,eliza_loop
    )
    ).

reply([check,my,disease]):-
    hypothesis(D),
    write('I believe you have: '),
    write(D),
    nl,
    undo,
    eliza_loop.

ask(Question):-
    write('Do you  have following symptoms: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
    ->
    assert(yes(Question));
    assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* how to verify something */
verify(S):-
    (yes(S)
    ->
    true;
    (   no(S)
    ->
    fail;
    ask(S))).

/* undo all yes/no assertion*/
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

/*stop eliza*/
reply([bye | _ ]):-
    write('Felt nice talking to you!, Bye....').

/*random reply if didnt understand query*/
reply([ Y | _ ]):-
    retract(confuse([ Next | Rest ])),
    append(Rest, [ Next ], NewExcuseList),
    asserta(confuse(NewExcuseList)),
    write(Next),nl,nl,
    eliza_loop.

/*reply if the query is not in brackets*/
reply(X):-
    write('please type your query in square brackets.'),nl,nl,
    eliza_loop.
















