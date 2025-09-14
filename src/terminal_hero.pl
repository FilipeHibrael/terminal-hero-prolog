:- module(terminal_hero, [
    play/0,
    start_game/1,
    game_loop/2
]).

:- use_module(game_config).
:- use_module(pure_game_logic).
:- use_module(input_handler).
:- use_module(display).
:- use_module(menu).

play :-
    at_halt(input_handler:restore_terminal(0)),
    menu:main_menu.

start_game(Difficulty) :-
    input_handler:setup_terminal,
    input_handler:flush_input,
    pure_game_logic:init_game_state(Difficulty, GameState),
    game_loop(0, GameState).

game_loop(Tick, GameStateIn) :-
    pure_game_logic:get_game_state_data(GameStateIn, Difficulty, _, _, _, _),
    game_config:difficulty(Difficulty, SleepTime, _, _, _),
    
    display:clear,
    pure_game_logic:update_game_state(Tick, GameStateIn, GameStateAfterUpdate),
    
    pure_game_logic:get_game_state_data(GameStateAfterUpdate, _, NewScore, NewComboCount, NewComboTotal, NewNotes),
    display:draw_game(NewScore, NewComboCount, NewComboTotal, NewNotes),
    
    input_handler:read_key(K),
    pure_game_logic:handle_input(K, GameStateAfterUpdate, GameStateAfterInput),

    flush_output,
    sleep(SleepTime),
    
    pure_game_logic:get_game_state_data(GameStateAfterInput, _, FinalScore, _, _, _),
    ( display:check_game_over(FinalScore) ->
        true
    ;
        T1 is Tick + 1,
        game_loop(T1, GameStateAfterInput)
    ).
