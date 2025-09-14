:- module(menu, [
    main_menu/0,
    difficulty_menu/0
]).

:- use_module('assets/title').
:- use_module(input_handler).
:- use_module(display).
:- use_module(terminal_hero).

main_menu :-
    display:clear,
    title:draw_title,
    format("~t~5+ ~n"),
    format("~t~5+1. Iniciar Jogo~n"),
    format("~t~5+2. Sair~n~n"),
    format("~t~5+Escolha uma opção: "),
    input_handler:read_menu_choice(Choice),
    handle_menu_choice(Choice).

handle_menu_choice(Choice) :-
    ( Choice = '\n' ->
        input_handler:read_menu_choice(NewChoice),
        handle_menu_choice(NewChoice)
    ; Choice = '1' ->
        difficulty_menu
    ; Choice = '2' ->
        format('~nSaindo do Terminal Hero...~n'),
        halt
    ; 
        format('Opção inválida! Tente novamente.~n'),
        sleep(1),
        main_menu
    ).

difficulty_menu :-
    display:clear,
    title:draw_title,
    format("~t~5+ ~n"),
    format("~t~5+Escolha a dificuldade:~n"),
    format("~t~5+1. Fácil~n"),
    format("~t~5+2. Médio~n"),
    format("~t~5+3. Difícil~n~n"),
    format("~t~5+Escolha uma opção: "),
    input_handler:read_menu_choice(Choice),
    handle_difficulty_choice(Choice).

handle_difficulty_choice(Choice) :-
    ( Choice = '\n' ->
        input_handler:read_menu_choice(NewChoice),
        handle_difficulty_choice(NewChoice)
    ; Choice = '1' -> 
        get_char(_), % Consume newline
        terminal_hero:start_game(easy)
    ; Choice = '2' -> 
        get_char(_), % Consume newline
        terminal_hero:start_game(medium)
    ; Choice = '3' -> 
        get_char(_), % Consume newline
        terminal_hero:start_game(hard)
    ; 
        format('Opção inválida! Tente novamente.~n'),
        sleep(1),
        difficulty_menu
    ).
