%% Input handling module
%% Manages keyboard input and terminal setup

:- module(input_handler, [
    setup_terminal/0,
    restore_terminal/1,
    read_key/1,
    read_menu_choice/1,
    handle_input/1
]).

:- use_module(game_state).
:- use_module(library(process)).
:- use_module(library(lists)).
:- use_module(library(readutil)).

%% Setup terminal for real-time input
setup_terminal :-
    shell('stty -icanon -echo < /dev/tty'),
    write('\e?25l').

%% Restore terminal to normal mode
restore_terminal(_) :-
    shell('stty icanon echo < /dev/tty'),
    write('\e[?25h'),
    format('~s', ['\e[0m']).

%% Read a single key with timeout
read_key(Key) :-
    Command = '(read -s -n 1 -t 0.05 key && echo -n "$key") || true',
    process_create(path(bash), ['-c', Command], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    ( Codes = [Code] -> char_code(Key, Code) ; Key = none ).

%% Read menu choice (blocking input)
read_menu_choice(Choice) :-
    % Force terminal restoration and ensure proper input mode
    shell('stty icanon echo < /dev/tty'),
    flush_output,
    get_char(user_input, Choice).





%% Handle game input during gameplay
handle_input(none) :- !.
handle_input(Key) :-
    downcase_atom(Key, K),
    findall(Y-InZone, game_state:note(K, Y, InZone), YsComZona),
    ( YsComZona = [] -> true
    ;
        keysort(YsComZona, Sorted),
        reverse(Sorted, [MaxY-InZone | _]),
        
        ( InZone == true ->
            once(retract(game_state:note(K, MaxY, _))),
            game_state:update_score(acerto)
        ;
            game_state:update_score(erro_apertar_cedo)
        )
    ).
