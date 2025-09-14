%% Main entry point for Terminal Hero
%% This file contains only the main execution logic

:- use_module('../src/terminal_hero').

%% Main entry point
main :-
    terminal_hero:play.

%% For SWI-Prolog initialization
:- initialization(main).
