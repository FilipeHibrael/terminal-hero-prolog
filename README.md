# Terminal Hero 🎵🎮

**Terminal Hero** é um jogo de ritmo simples feito em Prolog, inspirado em jogos como Guitar Hero, rodando diretamente no terminal.

Este projeto agora possui uma arquitetura modular baseada na estrutura de projetos Haskell, com separação clara de responsabilidades entre módulos.

---

## 📁 Estrutura do Projeto

```
.
├── app/
│   └── main.pl              # Ponto de entrada principal
├── src/
│   ├── assets/
│   │   └── title.pl         # Gráficos ASCII e título
│   ├── game_config.pl       # Configurações e constantes do jogo
│   ├── game_state.pl        # Gerenciamento de estado do jogo
│   ├── input_handler.pl     # Manipulação de entrada do teclado
│   ├── display.pl           # Sistema de renderização
│   ├── menu.pl             # Sistema de menus
│   └── terminal_hero.pl     # Módulo principal do jogo
├── tests/
│   └── tests.pl            # Suíte de testes
├── Makefile                # Comandos de build e execução
├── README.md               # Este arquivo
└── terminal-hero.pl        # Versão monolítica original (compatibilidade)
```

---

## 📦 Requisitos do Sistema

Certifique-se de estar em um sistema baseado em Unix (como Ubuntu) e ter permissões para instalar pacotes e executar scripts.

---

## 🚀 Instalação
Siga os passos abaixo para configurar o ambiente Prolog e executar o projeto.

### Instalando o SWI-Prolog (compilador e ambiente de desenvolvimento)
A forma mais fácil de instalar o SWI-Prolog é através do site oficial.

Linux (usando o gerenciador de pacotes da sua distribuição):

Para distribuições baseadas em Debian/Ubuntu:
```bash
sudo apt-get update
sudo apt-get install swi-prolog
```
Para distribuições baseadas em Fedora:
```bash
sudo dnf install swi-prolog
```
Dica: Após a instalação, feche e reabra o terminal para garantir que o comando swipl esteja disponível no seu PATH.

## 🔧 Execução do Projeto

### Usando o Makefile (Recomendado)

O projeto inclui um Makefile com comandos convenientes:

```bash
# Executar o jogo
make run

# Executar os testes
make test

# Limpar arquivos compilados
make clean

# Ver ajuda
make help
```

### Execução Manual

Navegue até o diretório do projeto e execute:

```bash
# Versão modular
swipl -g main -t halt app/main.pl

# Versão original (compatibilidade)
swipl -g play -t halt terminal-hero.pl
```

### Execução Interativa

Para executar no modo interativo do SWI-Prolog:

```bash
swipl app/main.pl
```

Depois digite:
```prolog
?- main.
```

---

## 🧪 Testes

Execute a suíte de testes para verificar se todos os módulos estão funcionando corretamente:

```bash
make test
```

Ou manualmente:
```bash
swipl -g run_tests -t halt tests/tests.pl
```

---

## 🏗️ Arquitetura Modular

### Módulos Principais:

- **app/main.pl**: Ponto de entrada que inicializa o jogo
- **src/terminal_hero.pl**: Coordenador principal e loop do jogo
- **src/game_config.pl**: Configurações de dificuldade e constantes
- **src/game_state.pl**: Gerenciamento de estado dinâmico (score, notas, combos)
- **src/input_handler.pl**: Captura e processamento de entrada do usuário
- **src/display.pl**: Renderização da interface do jogo
- **src/menu.pl**: Sistema de menus e navegação
- **src/assets/title.pl**: Recursos visuais (ASCII art)

### Vantagens da Modularização:

1. **Separação de Responsabilidades**: Cada módulo tem uma função específica
2. **Manutenibilidade**: Facilita correções e melhorias
3. **Testabilidade**: Permite testes unitários por módulo
4. **Reutilização**: Módulos podem ser reutilizados em outros projetos
5. **Legibilidade**: Código mais organizado e fácil de entender

Agora você está pronto para jogar! Divirta-se.

---

## 🕹️ Controles (Exemplo)

> (Dependente de como o jogo foi implementado)

- Teclas: `A`, `S`, `J`, `K` para acertar as notas.
- Pontuação é mostrada no canto superior da interface.
- Notas caem de cima e devem ser pressionadas no tempo certo.

---

### Docs com funcionalidades do projeto
https://docs.google.com/document/d/1ur6RbqMDHaa-zNUNkXIWdY6Q51y-UFhaHQI1seneUY8/edit?tab=t.0

