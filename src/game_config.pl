:- module(game_config, [
    difficulty/5,
    field_height/1,
    hit_zone_start/1,
    hit_zone_end/1,
    combo_goal/1,
    col_index/2,
    color/2,
    reset_color/1
]).

difficulty(easy,   0.20, 4, 10, -2).
difficulty(medium, 0.15, 3, 7,  -4).
difficulty(hard,   0.10, 2, 5,  -5).

field_height(19).
hit_zone_start(17).
hit_zone_end(22).
combo_goal(5).

col_index(a, 0). col_index(s, 1). col_index(j, 2). col_index(k, 3).

color(a, "\033[1;31m"). color(s, "\033[1;32m").
color(j, "\033[1;33m"). color(k, "\033[1;34m").
reset_color("\033[0m").
