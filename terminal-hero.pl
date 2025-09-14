:- use_module(library(process)).
:- use_module(library(random)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

dificuldade(facil,   0.20, 4, 10, -2).
dificuldade(medio,  0.15, 3, 7,  -4).
dificuldade(dificil, 0.10, 2, 5,  -5).

altura_pista(19).
% --- MODIFICADO: O alcance do acerto foi aumentado ---
zona_acerto_inicio(17). % Valor original era 19. Agora o jogador pode acertar mais cedo.
zona_acerto_fim(22).
combo_meta(5).

:- dynamic note/3. 
:- dynamic score/1.
:- dynamic dificuldade_atual/1.
:- dynamic combo_count/1.
:- dynamic combo_total/1.

% --- MENU PRINCIPAL ---
play :-
    at_halt(restore_terminal(0)),
    main_menu.

main_menu :-
    clear,
    draw_title,
    format("~t~5+ ~n"),
    format("~t~5+1. Iniciar Jogo~n"),
    format("~t~5+2. Sair~n~n"),
    format("~t~5+Escolha uma opção: "),
    read_menu_choice(Choice),
    handle_menu_choice(Choice).

handle_menu_choice(Choice) :-
    ( Choice = '\n' ->
        % Skip newline, read again
        read_menu_choice(NewChoice),
        handle_menu_choice(NewChoice)
    ; Choice = '1' ->
        menu_dificuldade
    ; Choice = '2' ->
        format('~nSaindo do Terminal Hero...~n'),
        halt
    ; 
        format('Opção inválida! Tente novamente.~n'),
        sleep(1),
        main_menu
    ).

menu_dificuldade :-
    clear,
    draw_title,
    format("~t~5+ ~n"),
    format("~t~5+Escolha a dificuldade:~n"),
    format("~t~5+1. Fácil~n"),
    format("~t~5+2. Médio~n"),
    format("~t~5+3. Difícil~n~n"),
    format("~t~5+Escolha uma opção: "),
    read_menu_choice(Choice),
    handle_difficulty_choice(Choice).

handle_difficulty_choice(Choice) :-
    ( Choice = '\n' ->
        % Skip newline, read again
        read_menu_choice(NewChoice),
        handle_difficulty_choice(NewChoice)
    ; Choice = '1' -> 
        start_game(facil)
    ; Choice = '2' -> 
        start_game(medio)
    ; Choice = '3' -> 
        start_game(dificil)
    ; 
        format('Opção inválida! Tente novamente.~n'),
        sleep(1),
        menu_dificuldade
    ).

read_menu_choice(Choice) :-
    % Force terminal restoration and ensure proper input mode
    shell('stty icanon echo < /dev/tty'),
    flush_output,
    get_char(user_input, Choice).

% --- TÍTULO ---
draw_title :-
    reset_color(Reset), color(a, C1),
    format('~s~s~n', [C1, "████████╗███████╗██████╗ ███╗   ███╗██╗███╗   ██╗ █████╗ ██╗     "]),
    format('~s~s~n', [C1, "╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║████╗  ██║██╔══██╗██║     "]),
    format('~s~s~n', [C1, "   ██║   █████╗  ██████╔╝██╔████╔██║██║██╔██╗ ██║███████║██║     "]),
    format('~s~s~n', [C1, "   ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║██║╚██╗██║██╔══██║██║     "]),
    format('~s~s~n', [C1, "   ██║   ███████╗██║  ██║██║ ╚═╝ ██║██║██║ ╚████║██║  ██║███████╗"]),
    format('~s~s~n', [C1, "   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝"]),
    format('~s~s~n', [C1, "        ██╗  ██╗███████╗██████╗  ██████╗          "]),
    format('~s~s~n', [C1, "        ██║  ██║██╔════╝██╔══██╗██╔═══██╗         "]),
    format('~s~s~n', [C1, "        ███████║█████╗  ██████╔╝██║   ██║         "]),
    format('~s~s~n', [C1, "        ██╔══██║██╔══╝  ██╔══██╗██║   ██║         "]),
    format('~s~s~n', [C1, "        ██║  ██║███████╗██║  ██║╚██████╔╝         "]),
    format('~s~s~n', [C1, "        ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝ ╚═════╝          "]),
    format('~n~s', [Reset]).

% --- LÓGICA PRINCIPAL DO JOGO ---
start_game(Dificuldade) :-
    setup_terminal,
    retractall(note(_,_,_)),
    retractall(score(_)),
    retractall(dificuldade_atual(_)),
    retractall(combo_count(_)),
    retractall(combo_total(_)),
    assertz(score(0)),
    assertz(combo_count(0)),
    assertz(combo_total(0)),
    assertz(dificuldade_atual(Dificuldade)),
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
    altura_pista(AlturaMax),
    zona_acerto_inicio(InicioZona),
    zona_acerto_fim(FimZona),
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

read_key(Key) :-
    Command = '(read -s -n 1 -t 0.05 key && echo -n "$key") || true',
    process_create(path(bash), ['-c', Command], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    ( Codes = [Code] -> char_code(Key, Code) ; Key = none ).

handle_input(none) :- !.
handle_input(Key) :-
    downcase_atom(Key, K),
    findall(Y-InZone, note(K, Y, InZone), YsComZona),
    ( YsComZona = [] -> true
    ;
        keysort(YsComZona, Sorted),
        reverse(Sorted, [MaxY-InZone | _]),
        
        ( InZone == true ->
            once(retract(note(K, MaxY, _))),
            update_score(acerto)
        ;
            update_score(erro_apertar_cedo)
        )
    ).

update_score(Evento) :-
    dificuldade_atual(D),
    score(S),
    score_change(Evento, D, Delta),
    S1 is S + Delta,
    retract(score(S)),
    assertz(score(S1)),
    update_combo(Evento),
    check_game_over(S1).

update_combo(acerto) :-
    combo_meta(Meta),
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

score_change(acerto, D, Pontos) :-
    dificuldade(D, _, _, Pontos, _).
score_change(erro_miss, D, Pontos) :-
    dificuldade(D, _, _, _, Pontos).
score_change(erro_apertar_cedo, _, -1).

check_game_over(S) :-
    ( S >= 100 -> format("~n🎸 Você venceu, YOU ROCK!~n"), halt
    ; S =< -20 -> format("~n💀 Game Over!~n"), halt
    ; true ).

clear :- write('\e[2J\e[H').

draw_game :-
    altura_pista(AlturaMax),
    score(S),
    combo_count(C),
    combo_total(T),
    dificuldade_atual(D),
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

draw_row(Y) :-
    forall(between(0, 3, Col),
        (
            ( col_index(Key, Col), note(Key, Y, InZone) ->
                color(Key, Color), reset_color(Reset),
                ( InZone == true -> Sym = '*' ; Sym = 'o' ),
                format("│~s ~w ~s", [Color, Sym, Reset])
            ;
                format("│   ")
            )
        )
    ),
    format("│").

game_loop(Tick) :-
    dificuldade_atual(Dificuldade),
    dificuldade(Dificuldade, SleepTime, SpawnRate, _, _),
    clear,
    move_notes,
    (Tick mod SpawnRate =:= 0 -> spawn_note ; true),
    draw_game,
    read_key(K),
    handle_input(K),
    flush_output,
    sleep(SleepTime),
    T1 is Tick + 1,
    game_loop(T1).