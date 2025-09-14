%% Title graphics and display module
%% Contains ASCII art and title-related display functions

:- module(title, [
    draw_title/0,
    draw_menu_header/0
]).

%% Draw the main Terminal Hero title
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

%% Draw a simplified header for menus
draw_menu_header :-
    reset_color(Reset), color(a, C1),
    format('~s~s~n', [C1, "╔════════════════════════════════════════╗"]),
    format('~s~s~n', [C1, "║            TERMINAL HERO               ║"]),
    format('~s~s~n', [C1, "╚════════════════════════════════════════╝"]),
    format('~n~s', [Reset]).

%% Color definitions (shared with main game)
color(a, "\033[1;31m"). color(s, "\033[1;32m").
color(j, "\033[1;33m"). color(k, "\033[1;34m").
reset_color("\033[0m").
