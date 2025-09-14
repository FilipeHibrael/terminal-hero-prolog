:- module(display, [
    clear/0,
    draw_game/4,
    draw_row/2,
    check_game_over/1
]).

:- use_module(game_config).

clear :-
    write('\e[2J\e[H').

draw_game(Score, ComboCount, ComboTotal, Notes) :-
    game_config:field_height(MaxHeight),
    format("+---------------------+~n"),
    format("| Score: ~|~` t~d~10+   |~n", [Score]),
    format("| Combo: ~|~` t~dx (~d)~10+|~n", [ComboCount, ComboTotal]),
    % format("| Dificuldade: ~|~` t~w~6+ |~n", [D]), % Difficulty is not passed
    format("|     a   s   j   k   |~n"),
    format("|   ┌───┬───┬───┬───┐ |~n"),
    forall(between(0, MaxHeight, Y),
        ( format("|   "), draw_row(Y, Notes), format(" |~n") )
    ),
    format("|   │===│===│===│===│ |~n"),
    format("|   └───┴───┴───┴───┘ |~n"),
    format("+---------------------+~n").

draw_row(Y, Notes) :-
    forall(between(0, 3, Col),
        (
            ( game_config:col_index(Key, Col), member(note(Key, Y, InZone), Notes) ->
                game_config:color(Key, Color), game_config:reset_color(Reset),
                ( InZone == true -> Sym = '*' ; Sym = 'o' ),
                format("│~s ~w ~s", [Color, Sym, Reset])
            ;
                format("│   ")
            )
        )
    ),
    format("│").

check_game_over(S) :-
    ( S >= 100 -> 
        format("~n🎸 Você venceu, YOU ROCK!~n"), 
        halt
    ; S =< -20 -> 
        format("~n💀 Game Over!~n"), 
        halt
    ; 
        fail 
    ).
