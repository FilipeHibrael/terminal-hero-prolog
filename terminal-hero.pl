:- use_module(library(random)).
:- use_module(library(readutil)).

:- dynamic note/3.
:- dynamic score/1.

% Início do jogo
start :-
    retractall(note(_,_,_)),
    retractall(score(_)),
    assertz(score(0)),
    format("~n🎸 Guitar Hero Terminal (Prolog Edition)~n"),
    format("Use as teclas A S J K para tocar (aperte Enter após cada tecla)~n"),
    sleep(1),
    game_loop(0).

% Mapeamento das colunas
col_index(a, 0).
col_index(s, 1).
col_index(j, 2).
col_index(k, 3).

% Cores ANSI
color(a, "\033[1;31m").  % Vermelho
color(s, "\033[1;32m").  % Verde
color(j, "\033[1;33m").  % Amarelo
color(k, "\033[1;34m").  % Azul
reset_color("\033[0m").

% Adiciona nota aleatória no topo
spawn_note :-
    random_member(Key, [a,s,j,k]),
    assertz(note(Key, 0, false)).

% Move as notas para baixo
move_notes :-
    findall((Key,Y,_), note(Key,Y,_), Notes),
    retractall(note(_,_,_)),
    forall(member((Key,Y,_), Notes),
        (
            Y1 is Y + 1,
            (Y1 >= 8 -> InZone = true ; InZone = false),
            (Y1 =< 9 -> assertz(note(Key,Y1,InZone)) ; true)
        )
    ).

% Leitura da tecla pressionada (precisa apertar Enter)
read_key(Key) :-
    read_line_to_codes(user_input, Codes),
    ( Codes = [Code|_] ->
        char_code(Key, Code)
    ; Key = '\n'
    ).

% Lida com a tecla pressionada
handle_input(Key) :-
    downcase_atom(Key, K),
    ( member(K, [a,s,j,k]) ->
        ( once(note(K, Y, true)) ->
            retract(note(K,Y,true)),
            update_score(10),
            format("~n✔️  Acertou! +10 pontos~n")
        ;
            update_score(-5),
            format("~n❌ Errou! -5 pontos~n")
        )
    ;
        true
    ).

% Atualiza o score
update_score(Delta) :-
    score(S),
    S1 is S + Delta,
    retract(score(S)),
    assertz(score(S1)),
    check_game_over(S1).

% Verifica vitória ou derrota
check_game_over(S) :-
    ( S >= 100 ->
        format("~n🎉 Você venceu!~n"),
        halt
    ; S =< -10 ->
        format("~n💀 Game Over!~n"),
        halt
    ; true ).

% Limpa a tela
clear :- write('\e[2J').

% Desenha o jogo
draw_game :-
    score(S),
    format("+---------------------+~n"),
    format("| Score: ~|~` t~d~6+      |~n", [S]),
    format("|                     |~n"),
    format("|     a   s   j   k   |~n"),
    format("|   ┌───┬───┬───┬───┐ |~n"),

    forall(between(0, 9, Y),
        (
            format("|   "),
            draw_row(Y),
            format(" |~n")
        )
    ),

    format("|   │===│===│===│===│ |~n"),
    format("|   └───┴───┴───┴───┘ |~n"),
    format("+---------------------+~n").

% Desenha uma linha Y
draw_row(Y) :-
    forall(between(0, 3, Col),
        (
            ( col_index(Key, Col),
              note(Key, Y, InZone) ->
                color(Key, Color),
                (InZone = true -> Sym = "*" ; Sym = "o"),
                format("│~s ~w ~s", [Color, Sym, "\033[0m"])
            ;
                format("│   ")
            )
        )
    ),
    format("│").

% Loop principal do jogo
game_loop(Tick) :-
    clear,
    move_notes,
    (Tick mod 3 =:= 0 -> spawn_note ; true),
    draw_game,
    read_key(K),
    handle_input(K),
    sleep(0.5),
    T1 is Tick + 1,
    game_loop(T1).
