%% Game configuration and constants
%% Contains all game parameters and difficulty settings

:- module(game_config, [
    dificuldade/5,
    altura_pista/1,
    zona_acerto_inicio/1,
    zona_acerto_fim/1,
    combo_meta/1,
    col_index/2,
    color/2,
    reset_color/1
]).

%% Difficulty settings: (Name, SleepTime, SpawnRate, Points, Miss_Penalty)
dificuldade(facil,   0.20, 4, 10, -2).
dificuldade(medio,  0.15, 3, 7,  -4).
dificuldade(dificil, 0.10, 2, 5,  -5).

%% Game field configuration
altura_pista(19).
zona_acerto_inicio(17). % Modified: increased hit zone range
zona_acerto_fim(22).
combo_meta(5).

%% Column mappings for keys
col_index(a, 0). col_index(s, 1). col_index(j, 2). col_index(k, 3).

%% Color definitions for each column
color(a, "\033[1;31m"). color(s, "\033[1;32m").
color(j, "\033[1;33m"). color(k, "\033[1;34m").
reset_color("\033[0m").
