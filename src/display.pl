%% Display and rendering module
%% Handles all game visual output

:- module(display, [
    clear/0,
    draw_game/0,
    draw_row/1,
    check_game_over/1
]).

:- use_module(game_config).
:- use_module(game_state).

%% Clear screen
clear :- write('\e[2J\e[H').

%% Draw the main game screen
draw_game :-
    game_config:altura_pista(AlturaMax),
    game_state:get_score(S),
    game_state:get_combo_info(C, T),
    game_state:get_difficulty(D),
    format("+---------------------+~n"),
    format("| Score: ~|~` t~d~10+   |~n", [S]),
    format("| Combo: ~|~` t~dx (~d)~10+|~n", [C, T]),
    format("| Dificuldade: ~|~` t~w~6+ |~n", [D]),
    format("|     a   s   j   k   |~n"),
    format("|   ┌───┬───┬───┬───┐ |~n"),
    forall(between(0, AlturaMax, Y),
        ( format("|   "), draw_row(Y), format(" |~n") )
    ),
    format("|   │===│===│===│===│ |~n"),
    format("|   └───┴───┴───┴───┘ |~n"),
    format("+---------------------+~n").

%% Draw a single row of the game field
draw_row(Y) :-
    forall(between(0, 3, Col),
        (
            ( game_config:col_index(Key, Col), game_state:note(Key, Y, InZone) ->
                game_config:color(Key, Color), game_config:reset_color(Reset),
                ( InZone == true -> Sym = '*' ; Sym = 'o' ),
                format("│~s ~w ~s", [Color, Sym, Reset])
            ;
                format("│   ")
            )
        )
    ),
    format("│").

%% Check for game over conditions
check_game_over(S) :-
    ( S >= 100 -> 
        format("~n🎸 Você venceu, YOU ROCK!~n"), 
        halt
    ; S =< -20 -> 
        format("~n💀 Game Over!~n"), 
        halt
    ; 
        true 
    ).
