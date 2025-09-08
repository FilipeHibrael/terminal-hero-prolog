:- module(terminal_hero, [main/0]).
:- use_module(library(thread)).

% ------------------------
% Configurações
% ------------------------
col_width(7).
columns(['a','s','j','k']).
height(25).
start_score(0).
game_over_limit(-10).

% ------------------------
% Estrutura de estado
% ------------------------
% game_state(Notes, Score, GameOver, TickIndex)

init_game(game_state([], Score, false, 0)) :-
    start_score(Score).

% ------------------------
% Notas determinísticas
% ------------------------
add_note_deterministic(Notes, TickIndex, NewNotes) :-
    columns(Cols),
    length(Cols, L),
    Ix is (TickIndex mod L) + 1,
    nth1(Ix, Cols, C),
    NewNotes = [note(C,0,false)|Notes].

move_notes_down([], []).
move_notes_down([note(C,Y,H)|Rest], [note(C,Y1,H)|Moved]) :-
    Y1 is Y + 1,
    move_notes_down(Rest, Moved).

clear_hit_flags([], []).
clear_hit_flags([note(C,Y,_)|Rest], [note(C,Y,false)|Cleaned]) :-
    clear_hit_flags(Rest, Cleaned).

valid_note(note(_, Y, _)) :-
    height(H),
    Y < H + 2.

filter_notes(Notes, Filtered) :-
    include(valid_note, Notes, Filtered).

% ------------------------
% Tick (atualização automática)
% ------------------------
tick(game_state(Notes, Score, GameOver, TickIndex),
     game_state(Filtered, Score, GameOver, NextIndex)) :-
    move_notes_down(Notes, Moved),
    add_note_deterministic(Moved, TickIndex, WithNew),
    filter_notes(WithNew, Filtered),
    NextIndex is TickIndex + 1.

% ------------------------
% Entrada do jogador
% ------------------------
apply_input(' ', game_state(_, _, true, TickIndex),
            game_state([], 0, false, TickIndex)). % Reinicia se GameOver

apply_input(C, game_state(Notes, Score, _, TickIndex),
            game_state(NewNotes, NewScore, NewGameOver, TickIndex)) :-
    columns(Cols),
    member(C, Cols),
    partition_notes_safe(Notes, C, Hit, Rest),
    mark_hits(Hit, MarkedHits),
    ( Hit = [] ->
        NewScore is Score - 1
    ; NewScore is Score + 1
    ),
    game_over_limit(Limit),
    ( NewScore < Limit -> NewGameOver = true ; NewGameOver = false ),
    append(MarkedHits, Rest, NewNotes).

apply_input(_, State, State). % Tecla inválida = nada

mark_hits([], []).
mark_hits([note(C,Y,_)|Rest], [note(C,Y,true)|Marked]) :-
    mark_hits(Rest, Marked).

% ------------------------
% Partition seguro (nunca falha)
% ------------------------
partition_notes_safe([], _, [], []).
partition_notes_safe([note(NC,Y,H)|Rest], Key, [note(NC,Y,H)|Hit], RestNotes) :-
    NC == Key,
    height(Ht),
    Y >= Ht - 2,
    Y =< Ht + 1,
    !,
    partition_notes_safe(Rest, Key, Hit, RestNotes).
partition_notes_safe([N|Rest], Key, Hit, [N|RestNotes]) :-
    partition_notes_safe(Rest, Key, Hit, RestNotes).

% ------------------------
% Game Step
% ------------------------
game_step(State, none, NextState) :-
    tick(State, Ticked),
    Ticked = game_state(Notes, Score, GO, TickIndex),
    clear_hit_flags(Notes, Cleared),
    NextState = game_state(Cleared, Score, GO, TickIndex).

game_step(State, some(Char), NextState) :-
    apply_input(Char, State, AfterInput),
    game_step(AfterInput, none, NextState).

% ------------------------
% Velocidade
% ------------------------
speed(Score, Micro) :-
    Base is 90000 - Score * 1000,
    ( Base < 20000 -> Micro = 20000 ; Micro = Base).

microsec_to_seconds(Micro, Sec) :-
    Sec is Micro / 1000000.0.

% ------------------------
% Renderização
% ------------------------
center_text(Str, Out) :-
    col_width(W),
    string_length(Str, L),
    Pad is W - L,
    Left is Pad // 2,
    Right is Pad - Left,
    length(LS, Left), maplist(=(' '), LS),
    length(RS, Right), maplist(=(' '), RS),
    string_chars(Str, Cs),
    append([LS, Cs, RS], OutCs),
    string_chars(Out, OutCs).

draw_cell(Col, Y, Notes, Out) :-
    ( member(note(Col,Y,true), Notes) -> Sym = "*"
    ; member(note(Col,Y,false), Notes) -> upcase_atom(Col, Sym)
    ; Sym = " "
    ),
    center_text(Sym, Out).

draw_key(C, Out) :-
    upcase_atom(C, CU),
    format(string(S), "[~w]", [CU]),
    center_text(S, Out).

draw_game(game_state(Notes, Score, GameOver, _)) :-
    ansi_clear_screen,
    format("Score: ~d~n", [Score]),
    columns(Cols),
    col_width(W),
    length(Cols, NCols),
    LineLen is NCols * W,
    forall(between(1, LineLen, _), write('-')), nl,
    height(H),
    forall(between(0,H,Y),
        ( forall(member(C,Cols),
                 ( draw_cell(C,Y,Notes,Out), write(Out) )),
          nl )),
    forall(between(1, LineLen, _), write('-')), nl,
    forall(member(C,Cols), ( draw_key(C,Out), write(Out) )), nl,nl,
    write("Use A, S, J, K para tocar. Espaço para reiniciar."), nl,
    ( GameOver == true -> ansi_format([fg(red)], "GAME OVER!~n", []); true ),
    flush_output.

ansi_clear_screen :-
    forall(between(1, 50, _), nl),
    flush_output.

% ------------------------
% Entrada com timeout (threads)
% ------------------------
start_input_thread(Me) :-
    thread_create(input_loop(Me), _, [detached(true)]).

input_loop(Me) :-
    with_tty_raw(loop_getchar(Me)).

loop_getchar(Me) :-
    get_single_char(Code),
    ( Code \= -1 ->
        char_code(C, Code),
        thread_send_message(Me, key(C))
    ; true ),
    loop_getchar(Me).

read_input_with_timeout(Me, Micro, none) :-
    Sec is Micro / 1000000.0,
    \+ thread_get_message(Me, key(_), [timeout(Sec)]), !.
read_input_with_timeout(Me, _, some(Char)) :-
    thread_get_message(Me, key(Char), [timeout(0)]).

% ------------------------
% Loop principal
% ------------------------
main_loop(Me, State) :-
    State = game_state(_, Score, _, _),
    speed(Score, Micro),
    read_input_with_timeout(Me, Micro, Input),
    game_step(State, Input, NextState),
    flush_output,
    draw_game(NextState),
    microsec_to_seconds(Micro, SleepSec),
    sleep(SleepSec),
    main_loop(Me, NextState).

% ------------------------
% Inicialização
% ------------------------
main :-
    thread_self(Me),
    start_input_thread(Me),
    init_game(State),
    main_loop(Me, State).
