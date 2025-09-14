:- module(tests, [
    run_all_tests/0,
    test_game_config/0,
    test_pure_game_logic/0,
    test_menu_logic/0,
    test_scoring_system/0,
    test_note_management/0,
    test_combo_system/0
]).

% Importar modulos necessários para os testes
:- use_module('../src/game_config').
:- use_module('../src/pure_game_logic').
:- use_module('../src/menu').

% Biblioteca para testes mais avançados
:- use_module(library(plunit)).

%% ============================================================================
%% MAIN TEST RUNNER
%% ============================================================================

run_all_tests :-
    format('~n=== EXECUTANDO TODOS OS TESTES DO TERMINAL HERO ===~n~n'),
    test_game_config,
    test_pure_game_logic,
    test_menu_logic,
    test_scoring_system,
    test_note_management,
    test_combo_system,
    format('~n=== TODOS OS TESTES CONCLUÍDOS ===~n').

%% ============================================================================
%% GAME CONFIG TESTS
%% ============================================================================

test_game_config :-
    format('--- Testando Configurações do Jogo ---~n'),
    test_difficulty_configs,
    test_field_dimensions,
    test_hit_zone_config,
    test_color_mappings,
    format('✓ Configurações do jogo: PASSOU~n~n').

test_difficulty_configs :-
    % Teste configurações de dificuldade
    game_config:difficulty(easy, SleepEasy, SpawnEasy, HitEasy, MissEasy),
    assertion(SleepEasy =:= 0.20),
    assertion(SpawnEasy =:= 4),
    assertion(HitEasy =:= 10),
    assertion(MissEasy =:= -2),
    
    game_config:difficulty(medium, SleepMed, SpawnMed, HitMed, MissMed),
    assertion(SleepMed =:= 0.15),
    assertion(SpawnMed =:= 3),
    assertion(HitMed =:= 7),
    assertion(MissMed =:= -4),
    
    game_config:difficulty(hard, SleepHard, SpawnHard, HitHard, MissHard),
    assertion(SleepHard =:= 0.10),
    assertion(SpawnHard =:= 2),
    assertion(HitHard =:= 5),
    assertion(MissHard =:= -5).

test_field_dimensions :-
    game_config:field_height(Height),
    assertion(Height =:= 19),
    
    game_config:hit_zone_start(Start),
    assertion(Start =:= 17),
    
    game_config:hit_zone_end(End),
    assertion(End =:= 22),
    
    % Verificar que a zona de hit faz sentido
    assertion(Start =< End),
    assertion(Start =< Height).

test_hit_zone_config :-
    game_config:combo_goal(Goal),
    assertion(Goal =:= 5).

test_color_mappings :-
    % Testar mapeamento de colunas
    game_config:col_index(a, ColA),
    game_config:col_index(s, ColS),
    game_config:col_index(j, ColJ),
    game_config:col_index(k, ColK),
    assertion(ColA =:= 0),
    assertion(ColS =:= 1),
    assertion(ColJ =:= 2),
    assertion(ColK =:= 3),
    
    % Testar códigos de cores
    game_config:color(a, ColorA),
    game_config:color(s, ColorS),
    game_config:color(j, ColorJ),
    game_config:color(k, ColorK),
    assertion(ColorA = "\033[1;31m"),
    assertion(ColorS = "\033[1;32m"),
    assertion(ColorJ = "\033[1;33m"),
    assertion(ColorK = "\033[1;34m"),
    
    game_config:reset_color(Reset),
    assertion(Reset = "\033[0m").

%% ============================================================================
%% PURE GAME LOGIC TESTS
%% ============================================================================

test_pure_game_logic :-
    format('--- Testando Lógica Pura do Jogo ---~n'),
    test_game_state_initialization,
    test_game_state_accessors,
    test_note_spawning,
    test_note_movement,
    format('✓ Lógica pura do jogo: PASSOU~n~n').

test_game_state_initialization :-
    % Testar inicialização do estado do jogo
    pure_game_logic:init_game_state(easy, GameState),
    pure_game_logic:get_game_state_data(GameState, Difficulty, Score, ComboCount, ComboTotal, Notes),
    
    assertion(Difficulty = easy),
    assertion(Score =:= 0),
    assertion(ComboCount =:= 0),
    assertion(ComboTotal =:= 0),
    assertion(Notes = []).

test_game_state_accessors :-
    % Testar acessores do estado do jogo
    pure_game_logic:init_game_state(medium, GameState),
    pure_game_logic:get_game_state_data(GameState, Difficulty, _Score, _ComboCount, _ComboTotal, Notes),
    
    assertion(Difficulty = medium),
    assertion(is_list(Notes)).

test_note_spawning :-
    % Testar spawn de notas (função interna, testada indiretamente)
    pure_game_logic:init_game_state(easy, InitialState),
    % Como spawn_note/2 é interna, testamos via update_game_state
    pure_game_logic:update_game_state(0, InitialState, UpdatedState), % Tick 0, deve spawnar
    pure_game_logic:get_game_state_data(UpdatedState, _, _, _, _, Notes),
    
    % Verificar que foi criada uma nota
    assertion(length(Notes, 1)),
    Notes = [note(Key, Y, InZone)],
    assertion(member(Key, [a, s, j, k])),
    assertion(Y =:= 0),
    assertion(InZone = false).

test_note_movement :-
    % Testar movimento de notas
    pure_game_logic:init_game_state(easy, InitialState),
    % Criar estado com uma nota manualmente para teste
    TestNote = note(a, 10, false),
    TestState = InitialState.put(notes, [TestNote]),
    
    % Mover notas (Tick não multiplo de spawn rate para não spawnar nova nota)
    pure_game_logic:update_game_state(1, TestState, UpdatedState),
    pure_game_logic:get_game_state_data(UpdatedState, _, _, _, _, [MovedNote]),
    
    MovedNote = note(a, Y, _),
    assertion(Y =:= 11). % Nota deve ter se movido para baixo

%% ============================================================================
%% MENU LOGIC TESTS
%% ============================================================================

test_menu_logic :-
    format('--- Testando Lógica do Menu ---~n'),
    % Testar se os predicados do menu existem e são carregáveis
    assertion(current_predicate(menu:main_menu/0)),
    assertion(current_predicate(menu:difficulty_menu/0)),
    format('✓ Lógica do menu: PASSOU~n~n').

%% ============================================================================
%% SCORING SYSTEM TESTS
%% ============================================================================

test_scoring_system :-
    format('--- Testando Sistema de Pontuação ---~n'),
    test_hit_scoring,
    test_miss_scoring,
    test_early_press_scoring,
    format('✓ Sistema de pontuação: PASSOU~n~n').

test_hit_scoring :-
    % Testar pontuação para acertos
    pure_game_logic:score_change(hit, easy, Points),
    assertion(Points =:= 10),
    
    pure_game_logic:score_change(hit, medium, Points2),
    assertion(Points2 =:= 7),
    
    pure_game_logic:score_change(hit, hard, Points3),
    assertion(Points3 =:= 5).

test_miss_scoring :-
    % Testar pontuação para erros de miss
    pure_game_logic:score_change(miss_error, easy, Points),
    assertion(Points =:= -2),
    
    pure_game_logic:score_change(miss_error, medium, Points2),
    assertion(Points2 =:= -4),
    
    pure_game_logic:score_change(miss_error, hard, Points3),
    assertion(Points3 =:= -5).

test_early_press_scoring :-
    % Testar pontuação para pressionar muito cedo
    pure_game_logic:score_change(early_press_error, easy, Points),
    assertion(Points =:= -1),
    
    pure_game_logic:score_change(early_press_error, medium, Points2),
    assertion(Points2 =:= -1),
    
    pure_game_logic:score_change(early_press_error, hard, Points3),
    assertion(Points3 =:= -1).

%% ============================================================================
%% NOTE MANAGEMENT TESTS
%% ============================================================================

test_note_management :-
    format('--- Testando Gerenciamento de Notas ---~n'),
    test_note_hit_detection,
    test_note_removal,
    format('✓ Gerenciamento de notas: PASSOU~n~n').

test_note_hit_detection :-
    % Testar detecção de acerto de notas
    pure_game_logic:init_game_state(easy, InitialState),
    
    % Criar uma nota na zona de hit
    game_config:hit_zone_start(ZoneStart),
    TestNote = note(a, ZoneStart, true),
    TestState = InitialState.put(notes, [TestNote]),
    
    % Tentar acertar a nota
    pure_game_logic:handle_input(a, TestState, ResultState),
    pure_game_logic:get_game_state_data(ResultState, _, Score, ComboCount, _, Notes),
    
    % Verificar que a nota foi removida e pontuação aumentou
    assertion(Notes = []),
    assertion(Score > 0),
    assertion(ComboCount =:= 1).

test_note_removal :-
    % Testar remoção de notas que saíram da tela
    pure_game_logic:init_game_state(easy, InitialState),
    
    % Criar uma nota que está muito embaixo (vai sair da tela)
    game_config:field_height(MaxHeight),
    TestNote = note(a, MaxHeight, false),
    TestState = InitialState.put(notes, [TestNote]),
    
    % Atualizar estado (nota deve ser removida)
    pure_game_logic:update_game_state(1, TestState, ResultState),
    pure_game_logic:get_game_state_data(ResultState, _, Score, _, _, Notes),
    
    % Verificar que a nota foi removida e houve penalidade
    assertion(Notes = []),
    assertion(Score < 0). % Miss error

%% ============================================================================
%% COMBO SYSTEM TESTS
%% ============================================================================

test_combo_system :-
    format('--- Testando Sistema de Combo ---~n'),
    test_combo_increment,
    test_combo_reset,
    test_combo_bonus,
    format('✓ Sistema de combo: PASSOU~n~n').

test_combo_increment :-
    % Testar incremento de combo
    pure_game_logic:init_game_state(easy, InitialState),
    game_config:hit_zone_start(ZoneStart),
    TestNote = note(a, ZoneStart, true),
    TestState = InitialState.put(notes, [TestNote]),
    
    pure_game_logic:handle_input(a, TestState, ResultState),
    pure_game_logic:get_game_state_data(ResultState, _, _, ComboCount, _, _),
    
    assertion(ComboCount =:= 1).

test_combo_reset :-
    % Testar reset de combo em erro
    pure_game_logic:init_game_state(easy, InitialState),
    
    % Primeiro, criar um combo
    TestNote1 = note(a, 18, true),
    TestState1 = InitialState.put(notes, [TestNote1]),
    pure_game_logic:handle_input(a, TestState1, StateWithCombo),
    
    % Agora causar um erro (pressionar tecla sem nota correspondente)
    pure_game_logic:handle_input(s, StateWithCombo, FinalState),
    pure_game_logic:get_game_state_data(FinalState, _, _, ComboCount, _, _),
    
    assertion(ComboCount =:= 0).

test_combo_bonus :-
    % Testar bônus de combo
    game_config:combo_goal(Goal),
    pure_game_logic:init_game_state(easy, InitialState),
    
    % Simular conseguir o objetivo de combo
    % (Isso requer uma sequência mais complexa de testes)
    TestState = InitialState.put([combo_count: Goal, score: 50]),
    
    % Simular um hit que deveria ativar o bônus
    game_config:hit_zone_start(ZoneStart),
    TestNote = note(a, ZoneStart, true),
    StateWithNote = TestState.put(notes, [TestNote]),
    
    pure_game_logic:handle_input(a, StateWithNote, FinalState),
    pure_game_logic:get_game_state_data(FinalState, _, FinalScore, FinalCombo, ComboTotal, _),
    
    % Verificar que combo foi resetado e total incrementado
    assertion(FinalCombo =:= 0),
    assertion(ComboTotal =:= 1),
    assertion(FinalScore > 50). % Deve ter recebido bônus

%% ============================================================================
%% UTILITY FUNCTIONS
%% ============================================================================

assertion(Goal) :-
    ( call(Goal) ->
        true
    ;
        format('FALHA NA ASSERÇÃO: ~w~n', [Goal]),
        fail
    ).

%% ============================================================================
%% INTEGRATION TESTS
%% ============================================================================

test_integration :-
    format('--- Testando Integração ---~n'),
    test_complete_game_cycle,
    format('✓ Integração: PASSOU~n~n').

test_complete_game_cycle :-
    % Testar um ciclo completo do jogo
    pure_game_logic:init_game_state(easy, InitialState),
    
    % Simular alguns ticks
    pure_game_logic:update_game_state(0, InitialState, State1), % Deve spawnar nota
    pure_game_logic:update_game_state(1, State1, State2),       % Move nota
    pure_game_logic:update_game_state(2, State2, State3),       % Move nota
    
    pure_game_logic:get_game_state_data(State3, _, _, _, _, Notes),
    assertion(length(Notes, 1)), % Deve ter uma nota
    
    % Tentar acertar a nota quando ela estiver na zona
    Notes = [note(Key, _Y, InZone)],
    ( InZone = true ->
        pure_game_logic:handle_input(Key, State3, FinalState),
        pure_game_logic:get_game_state_data(FinalState, _, Score, ComboCount, _, FinalNotes),
        assertion(Score > 0),
        assertion(ComboCount =:= 1),
        assertion(FinalNotes = [])
    ;
        true % Nota ainda não está na zona, isso é normal
    ).