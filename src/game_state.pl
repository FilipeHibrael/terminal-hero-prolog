%% Game state management
%% Handles all dynamic game state including score, notes, and combos

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
    dificuldade_atual/1,
    combo_count/1,
    combo_total/1
]).

:- use_module(game_config).
:- use_module(library(random)).
:- use_module(library(lists)).

%% Dynamic predicates for game state
:- dynamic note/3.          % note(Key, Y_Position, InZone)
:- dynamic score/1.         % score(CurrentScore)
:- dynamic dificuldade_atual/1.  % dificuldade_atual(Difficulty)
:- dynamic combo_count/1.   % combo_count(CurrentCombo)
:- dynamic combo_total/1.   % combo_total(TotalCombos)

%% Initialize game state for a new game
init_game_state(Dificuldade) :-
    cleanup_game_state,
    assertz(score(0)),
    assertz(combo_count(0)),
    assertz(combo_total(0)),
    assertz(dificuldade_atual(Dificuldade)).

%% Clean up all dynamic predicates
cleanup_game_state :-
    retractall(note(_,_,_)),
    retractall(score(_)),
    retractall(dificuldade_atual(_)),
    retractall(combo_count(_)),
    retractall(combo_total(_)).

%% Spawn a new note at the top
spawn_note :-
    random_member(Key, [a,s,j,k]),
    assertz(note(Key, 0, false)).

%% Move all notes down one position
move_notes :-
    game_config:altura_pista(AlturaMax),
    game_config:zona_acerto_inicio(InicioZona),
    game_config:zona_acerto_fim(FimZona),
    findall((Key,Y), note(Key,Y,_), Notes),
    retractall(note(_,_,_)),
    forall(member((Key,Y), Notes),
        (
            Y1 is Y + 1,
            ( Y == AlturaMax -> update_score(erro_miss) ; true ),
            ( between(InicioZona, FimZona, Y1) -> NewInZone = true ; NewInZone = false ),
            ( Y1 =< AlturaMax -> assertz(note(Key, Y1, NewInZone)) ; true)
        )
    ).

%% Update score based on events
update_score(Evento) :-
    dificuldade_atual(D),
    score(S),
    score_change(Evento, D, Delta),
    S1 is S + Delta,
    retract(score(S)),
    assertz(score(S1)),
    update_combo(Evento).

%% Calculate score changes based on events
score_change(acerto, D, Pontos) :-
    game_config:dificuldade(D, _, _, Pontos, _).
score_change(erro_miss, D, Pontos) :-
    game_config:dificuldade(D, _, _, _, Pontos).
score_change(erro_apertar_cedo, _, -1).

%% Update combo counter
update_combo(acerto) :-
    game_config:combo_meta(Meta),
    retract(combo_count(C)),
    C1 is C + 1,
    ( C1 >= Meta ->
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
update_combo(erro_miss) :-
    retractall(combo_count(_)),
    assertz(combo_count(0)).
update_combo(erro_apertar_cedo) :-
    retractall(combo_count(_)),
    assertz(combo_count(0)).

%% Getters for game state
get_score(S) :- score(S).
get_combo_info(C, T) :- combo_count(C), combo_total(T).
get_difficulty(D) :- dificuldade_atual(D).
