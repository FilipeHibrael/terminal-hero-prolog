:- module(input_handler, [
    setup_terminal/0,
    restore_terminal/1,
    read_key/1,
    read_menu_choice/1,
    flush_input/0
]).

:- use_module(library(process)).
:- use_module(library(readutil)).

setup_terminal :-
    shell('stty -icanon -echo < /dev/tty'),
    write('\e?25l').

restore_terminal(_) :-
    shell('stty icanon echo < /dev/tty'),
    write('\e[?25h'),
    format('~s', ['\e[0m']).

read_key(Key) :-
    Command = '(read -s -n 1 -t 0.05 key && echo -n "$key") || true',
    process_create(path(bash), ['-c', Command], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    ( Codes = [Code] -> char_code(Key, Code) ; Key = none ).

read_menu_choice(Choice) :-
    shell('stty icanon echo < /dev/tty'),
    flush_output,
    get_char(user_input, Choice).

flush_input :-
    read_pending_chars(user_input, _, _).
