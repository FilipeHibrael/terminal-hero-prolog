:- module(terminal_hero, [
    play/0,
    start_game/1,
    game_loop/1
]).

:- use_module(game_config).
:- use_module(game_state).
:- use_module(input_handler).
:- use_module(display).
:- use_module(menu).

play :-
    at_halt(input_handler:restore_terminal(0)),
    menu:main_menu.

start_game(Dificuldade) :-
    input_handler:setup_terminal,
    game_state:init_game_state(Dificuldade),
    game_loop(0).

game_loop(Tick) :-
    game_state:get_difficulty(Dificuldade),
    game_config:dificuldade(Dificuldade, SleepTime, SpawnRate, _, _),
    display:clear,
    game_state:move_notes,
    (Tick mod SpawnRate =:= 0 -> game_state:spawn_note ; true),
    display:draw_game,
    input_handler:read_key(K),
    input_handler:handle_input(K),
    flush_output,
    sleep(SleepTime),
    game_state:get_score(S),
    display:check_game_over(S),
    T1 is Tick + 1,
    game_loop(T1).
