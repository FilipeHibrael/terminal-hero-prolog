%% Menu system module
%% Handles main menu and difficulty selection

:- module(menu, [
    main_menu/0,
    menu_dificuldade/0
]).

:- use_module('assets/title').
:- use_module(input_handler).
:- use_module(display).
:- use_module(terminal_hero).

%% Main menu display and handling
main_menu :-
    display:clear,
    title:draw_title,
    format("~t~5+ ~n"),
    format("~t~5+1. Iniciar Jogo~n"),
    format("~t~5+2. Sair~n~n"),
    format("~t~5+Escolha uma opção: "),
    input_handler:read_menu_choice(Choice),
    handle_menu_choice(Choice).

%% Handle main menu choices
handle_menu_choice(Choice) :-
    ( Choice = '\n' ->
        % Skip newline, read again
        input_handler:read_menu_choice(NewChoice),
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

%% Difficulty selection menu
menu_dificuldade :-
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

%% Handle difficulty choices
handle_difficulty_choice(Choice) :-
    ( Choice = '\n' ->
        % Skip newline, read again
        input_handler:read_menu_choice(NewChoice),
        handle_difficulty_choice(NewChoice)
    ; Choice = '1' -> 
        terminal_hero:start_game(facil)
    ; Choice = '2' -> 
        terminal_hero:start_game(medio)
    ; Choice = '3' -> 
        terminal_hero:start_game(dificil)
    ; 
        format('Opção inválida! Tente novamente.~n'),
        sleep(1),
        menu_dificuldade
    ).
