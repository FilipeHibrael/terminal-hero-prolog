:- module(pure_game_logic, [
    init_game_state/2,
    update_game_state/3,
    handle_input/3,
    get_game_state_data/6
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(game_config).

% GameState = gameState{notes: list, score: int, combo_count: int, combo_total: int, difficulty: atom}

init_game_state(Difficulty, GameState) :-
    GameState = gameState{
        notes: [],
        score: 0,
        combo_count: 0,
        combo_total: 0,
        difficulty: Difficulty
    }.

update_game_state(Tick, GameStateIn, GameStateOut) :-
    move_notes(GameStateIn, GameStateAfterMove),
    (
        game_config:difficulty(GameStateIn.difficulty, _, SpawnRate, _, _),
        Tick mod SpawnRate =:= 0 ->
        spawn_note(GameStateAfterMove, GameStateOut)
    ;
        GameStateOut = GameStateAfterMove
    ).

spawn_note(GameStateIn, GameStateOut) :-
    random_member(Key, [a, s, j, k]),
    NewNote = note(Key, 0, false),
    NewNotes = [NewNote | GameStateIn.notes],
    GameStateOut = GameStateIn.put(notes, NewNotes).

move_notes(GameStateIn, GameStateOut) :-
    move_notes_recursive(GameStateIn.notes, NewNotes, ScoreDelta, GameStateIn.difficulty),
    NewScore is GameStateIn.score + ScoreDelta,
    ( ScoreDelta < 0 ->
        NewComboCount = 0
    ;
        NewComboCount = GameStateIn.combo_count
    ),
    GameStateOut = GameStateIn.put([
        notes: NewNotes,
        score: NewScore,
        combo_count: NewComboCount
    ]).

move_notes_recursive([], [], 0, _).
move_notes_recursive([note(Key, Y, _) | RestIn], NotesOut, TotalDelta, Difficulty) :-
    game_config:field_height(MaxHeight),
    game_config:hit_zone_start(ZoneStart),
    game_config:hit_zone_end(ZoneEnd),
    Y1 is Y + 1,
    move_notes_recursive(RestIn, RestOut, RestDelta, Difficulty),
    ( Y1 > MaxHeight ->
        score_change(miss_error, Difficulty, CurrentDelta),
        NotesOut = RestOut
    ;
        CurrentDelta is 0,
        ( between(ZoneStart, ZoneEnd, Y1) -> InZone = true ; InZone = false ),
        NotesOut = [note(Key, Y1, InZone) | RestOut]
    ),
    TotalDelta is CurrentDelta + RestDelta.

handle_input(Key, GameStateIn, GameStateOut) :-
    ( member(Key, [a, s, j, k]) ->
        process_hit(Key, GameStateIn, GameStateOut)
    ;
        GameStateOut = GameStateIn
    ).

process_hit(Key, GameStateIn, GameStateOut) :-
    game_config:hit_zone_start(ZoneStart),
    game_config:hit_zone_end(ZoneEnd),
    ( select(note(Key, Y, true), GameStateIn.notes, RemainingNotes), between(ZoneStart, ZoneEnd, Y) ->
        update_score_and_combo(hit, GameStateIn, GameStateWithUpdate),
        GameStateOut = GameStateWithUpdate.put(notes, RemainingNotes)
    ;
        update_score_and_combo(early_press_error, GameStateIn, GameStateOut)
    ).

update_score_and_combo(Event, GameStateIn, GameStateOut) :-
    score_change(Event, GameStateIn.difficulty, Delta),
    NewScore is GameStateIn.score + Delta,
    ( Event = hit ->
        NewComboCount is GameStateIn.combo_count + 1,
        game_config:combo_goal(Goal),
        ( NewComboCount >= Goal ->
            BonusScore is NewScore + NewComboCount,
            FinalScore = BonusScore,
            FinalComboCount = 0,
            FinalComboTotal is GameStateIn.combo_total + 1
        ;
            FinalScore = NewScore,
            FinalComboCount = NewComboCount,
            FinalComboTotal = GameStateIn.combo_total
        )
    ;
        FinalScore = NewScore,
        FinalComboCount = 0,
        FinalComboTotal = GameStateIn.combo_total
    ),
    GameStateOut = GameStateIn.put([
        score: FinalScore,
        combo_count: FinalComboCount,
        combo_total: FinalComboTotal
    ]).

score_change(hit, D, Points) :-
    game_config:difficulty(D, _, _, Points, _).
score_change(miss_error, D, Points) :-
    game_config:difficulty(D, _, _, _, Points).
score_change(early_press_error, _, -1).

get_game_state_data(GameState, Difficulty, Score, ComboCount, ComboTotal, Notes) :-
    Difficulty = GameState.difficulty,
    Score = GameState.score,
    ComboCount = GameState.combo_count,
    ComboTotal = GameState.combo_total,
    Notes = GameState.notes.
