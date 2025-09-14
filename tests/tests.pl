%% Test suite for Terminal Hero
%% Contains unit tests for various game components

:- use_module('../src/game_config').
:- use_module('../src/game_state').
:- use_module('../src/display').

test_game_config :-
    format('Testing game configuration...~n'),
    game_config:difficulty(easy, Sleep, Spawn, Points, Miss),
    format('Easy difficulty: Sleep=~w, Spawn=~w, Points=~w, Miss=~w~n', 
           [Sleep, Spawn, Points, Miss]),
    game_config:field_height(Height),
    format('Field height: ~w~n', [Height]),
    format('Game config tests passed!~n~n').

test_game_state :-
    format('Testing game state...~n'),
    game_state:init_game_state(easy),
    game_state:get_score(S),
    format('Initial score: ~w~n', [S]),
    game_state:get_combo_info(C, T),
    format('Initial combo: ~w (~w total)~n', [C, T]),
    game_state:get_difficulty(D),
    format('Difficulty: ~w~n', [D]),
    game_state:cleanup_game_state,
    format('Game state tests passed!~n~n').

test_score_updates :-
    format('Testing score updates...~n'),
    game_state:init_game_state(easy),
    game_state:get_score(S1),
    format('Score before hit: ~w~n', [S1]),
    game_state:update_score(hit),
    game_state:get_score(S2),
    format('Score after hit: ~w~n', [S2]),
    game_state:update_score(miss_error),
    game_state:get_score(S3),
    format('Score after miss: ~w~n', [S3]),
    game_state:cleanup_game_state,
    format('Score update tests passed!~n~n').

test_notes :-
    format('Testing note mechanics...~n'),
    game_state:init_game_state(medium),
    game_state:spawn_note,
    findall(Key-Y, game_state:note(Key, Y, _), Notes1),
    format('Notes after spawn: ~w~n', [Notes1]),
    game_state:move_notes,
    findall(Key-Y, game_state:note(Key, Y, _), Notes2),
    format('Notes after move: ~w~n', [Notes2]),
    game_state:cleanup_game_state,
    format('Note mechanics tests passed!~n~n').

%% Run all tests
run_tests :-
    format('=== Terminal Hero Test Suite ===~n~n'),
    test_game_config,
    test_game_state,
    test_score_updates,
    test_notes,
    format('=== All tests completed! ===~n').

%% Entry point for running tests
:- initialization(run_tests).
