:- use_module(library(process)).
:- use_module(library(random)).
:- use_module(library(readutil)).

:- dynamic note/3.
:- dynamic score/1.

% MENU PRINCIPAL
play :-
    at_halt(restore_terminal(0)),
    main_menu.

main_menu :-
    setup_terminal,
    clear,
    draw_title,
    format("~t~5+ ~n"),
    format("~t~5+1. Iniciar Jogo~n"),
    format("~t~5+2. Sair~n~n"),
    format("~t~5+Escolha uma opção: "),
    read_menu_choice(Choice),
    handle_menu_choice(Choice).

handle_menu_choice('1') :-
    start_game.
handle_menu_choice('2') :-
    format('~nSaindo do Terminal Hero...~n'),
    halt.
handle_menu_choice(_) :-
    format('Opção inválida! Tente novamente.~n'),
    sleep(1),
    main_menu.

read_menu_choice(Choice) :-
    flush_output,
    read_line_to_codes(user_input, Codes),
    filter_whitespace_codes(Codes, FilteredCodes),
    handle_filtered_codes(FilteredCodes, Choice).

filter_whitespace_codes(Codes, Filtered) :-
    exclude(is_whitespace, Codes, Filtered).

is_whitespace(10). % Newline (Enter)
is_whitespace(13). % Carriage Return
is_whitespace(32). % Space
is_whitespace(9).  % Tab

handle_filtered_codes([Code], Choice) :- !,
    char_code(Choice, Code).
handle_filtered_codes(_, 'invalid').

% TÍTULO ASCII
draw_title :-
    reset_color(Reset), color(a, C1),
    % Linhas 1-3 em Amarelo
    format('~s~s~n', [C1, "████████╗███████╗██████╗ ███╗   ███╗██╗███╗   ██╗ █████╗ ██╗     "]),
    format('~s~s~n', [C1, "╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║████╗  ██║██╔══██╗██║     "]),
    format('~s~s~n', [C1, "   ██║   █████╗  ██████╔╝██╔████╔██║██║██╔██╗ ██║███████║██║     "]),
    % Linhas 4-6 em Verde
    format('~s~s~n', [C1, "   ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║██║╚██╗██║██╔══██║██║     "]),
    format('~s~s~n', [C1, "   ██║   ███████╗██║  ██║██║ ╚═╝ ██║██║██║ ╚████║██║  ██║███████╗"]),
    format('~s~s~n', [C1, "   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝"]),
    % Linhas 7-9 em Vermelho
    format('~s~s~n', [C1, "        ██╗  ██╗███████╗██████╗  ██████╗          "]),
    format('~s~s~n', [C1, "        ██║  ██║██╔════╝██╔══██╗██╔═══██╗         "]),
    format('~s~s~n', [C1, "        ███████║█████╗  ██████╔╝██║   ██║         "]),
    % Linhas 10-12 em Azul
    format('~s~s~n', [C1, "        ██╔══██║██╔══╝  ██╔══██╗██║   ██║         "]),
    format('~s~s~n', [C1, "        ██║  ██║███████╗██║  ██║╚██████╔╝         "]),
    format('~s~s~n', [C1, "        ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝ ╚═════╝          "]),
    format('~n~s', [Reset]).

% LÓGICA PRINCIPAL DO JOGO
start_game :-
    retractall(note(_,_,_)),
    retractall(score(_)),
    assertz(score(0)),
    game_loop(0).

setup_terminal :-
    shell('stty -icanon -echo < /dev/tty'),
    write('\e?25l').

restore_terminal(_) :-
    shell('stty icanon echo < /dev/tty'),
    write('\e[?25h'),
    format('~s', ['\e[0m']).

col_index(a, 0). col_index(s, 1). col_index(j, 2). col_index(k, 3).
color(a, "\033[1;31m"). color(s, "\033[1;32m").
color(j, "\033[1;33m"). color(k, "\033[1;34m").
reset_color("\033[0m").

spawn_note :-
    random_member(Key, [a,s,j,k]),
    assertz(note(Key, 0, false)).

move_notes :-
    findall((Key,Y,InZone), note(Key,Y,InZone), Notes),
    retractall(note(_,_,_)),
    forall(member((Key,Y,InZone), Notes),
        (
            Y1 is Y + 1,
            ( Y == 9, InZone == true -> update_score(-5) ; true ),
            (Y1 >= 7 -> NewInZone = true ; NewInZone = false),
            (Y1 =< 9 -> assertz(note(Key,Y1,NewInZone)) ; true)
        )
    ).

read_key(Key) :-
    Command = '(read -s -n 1 -t 0.05 key && echo -n "$key") || true',
    process_create(path(bash), ['-c', Command], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    ( Codes = [Code] -> char_code(Key, Code) ; Key = none ).

handle_input(none) :- !.
handle_input(Key) :-
    downcase_atom(Key, K),
    findall(Y, note(K, Y, _), Ys),
    ( Ys \= [] ->
        max_list(Ys, MaxY),
        once(retract(note(K, MaxY, _))),
        ( MaxY >= 7 -> update_score(10) ; update_score(-2) )
    ;
        true
    ).

update_score(Delta) :-
    score(S),
    S1 is S + Delta,
    retract(score(S)),
    assertz(score(S1)),
    check_game_over(S1).

check_game_over(S) :-
    ( S >= 100 -> format("~n🎸 Você venceu, YOU ROCK!~n"), halt
    ; S =< -10 -> format("~n💀 Game Over!~n"), halt
    ; true ).

clear :- write('\e[2J\e[H').

draw_game :-
    score(S),
    format("+---------------------+~n"),
    format("| Score: ~|~` t~d~6+      |~n", [S]),
    format("|                     |~n"),
    format("|     a   s   j   k   |~n"),
    format("|   ┌───┬───┬───┬───┐ |~n"),
    forall(between(0, 9, Y),
        ( format("|   "), draw_row(Y), format(" |~n") )
    ),
    format("|   │===│===│===│===│ |~n"),
    format("|   └───┴───┴───┴───┘ |~n"),
    format("+---------------------+~n").

draw_row(Y) :-
    forall(between(0, 3, Col),
        (
            ( col_index(Key, Col), note(Key, Y, InZone) ->
                color(Key, Color), reset_color(Reset),
                (InZone = true -> Sym = "*" ; Sym = "o"),
                format("│~s ~w ~s", [Color, Sym, Reset])
            ;
                format("│   ")
            )
        )
    ),
    format("│").

game_loop(Tick) :-
    clear,
    move_notes,
    (Tick mod 3 =:= 0 -> spawn_note ; true),
    draw_game,
    read_key(K),
    handle_input(K),
    flush_output,
    sleep(0.18),
    T1 is Tick + 1,
    game_loop(T1).