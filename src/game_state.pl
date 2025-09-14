:- module(game_state, [
    init_game_state/1,
    cleanup_game_state/0,
    spawn_note/0,
    move_notes/0,
    update_score/1,
    get_score/1,
    get_combo_info/2,
    get_difficulty/1,
    note/3,
    score/1,
    current_difficulty/1,
    combo_count/1,
    combo_total/1
]).

:- use_module(game_config).
:- use_module(library(random)).
:- use_module(library(lists)).

:- dynamic note/3.
:- dynamic score/1.
:- dynamic current_difficulty/1.
:- dynamic combo_count/1.
:- dynamic combo_total/1.

init_game_state(Difficulty) :-
    cleanup_game_state,
    assertz(score(0)),
    assertz(combo_count(0)),
    assertz(combo_total(0)),
    assertz(current_difficulty(Difficulty)).

cleanup_game_state :-
    retractall(note(_,_,_)),
    retractall(score(_)),
    retractall(current_difficulty(_)),
    retractall(combo_count(_)),
    retractall(combo_total(_)).

spawn_note :-
    random_member(Key, [a,s,j,k]),
    assertz(note(Key, 0, false)).

move_notes :-
    game_config:field_height(MaxHeight),
    game_config:hit_zone_start(ZoneStart),
    game_config:hit_zone_end(ZoneEnd),
    findall((Key,Y), note(Key,Y,_), Notes),
    retractall(note(_,_,_)),
    forall(member((Key,Y), Notes),
        (
            Y1 is Y + 1,
            ( Y == MaxHeight -> update_score(miss_error) ; true ),
            ( between(ZoneStart, ZoneEnd, Y1) -> NewInZone = true ; NewInZone = false ),
            ( Y1 =< MaxHeight -> assertz(note(Key, Y1, NewInZone)) ; true)
        )
    ).

update_score(Event) :-
    current_difficulty(D),
    score(S),
    score_change(Event, D, Delta),
    S1 is S + Delta,
    retract(score(S)),
    assertz(score(S1)),
    update_combo(Event).

score_change(hit, D, Points) :-
    game_config:difficulty(D, _, _, Points, _).
score_change(miss_error, D, Points) :-
    game_config:difficulty(D, _, _, _, Points).
score_change(early_press_error, _, -1).

update_combo(hit) :-
    game_config:combo_goal(Goal),
    retract(combo_count(C)),
    C1 is C + 1,
    ( C1 >= Goal ->
        score(S),
        S_bonus is S + C1,
        retract(score(S)),
        assertz(score(S_bonus)),
        retract(combo_total(T)),
        T1 is T + 1,
        assertz(combo_total(T1)),
        assertz(combo_count(0))
    ;
        assertz(combo_count(C1))
    ).
update_combo(miss_error) :-
    retractall(combo_count(_)),
    assertz(combo_count(0)).
update_combo(early_press_error) :-
    retractall(combo_count(_)),
    assertz(combo_count(0)).

get_score(S) :- score(S).
get_combo_info(C, T) :- combo_count(C), combo_total(T).
get_difficulty(D) :- current_difficulty(D).
