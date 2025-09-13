# Terminal Hero 🎵🎮

**Terminal Hero** é um jogo de ritmo simples feito em Haskell, inspirado em jogos como Guitar Hero, rodando diretamente no terminal com interface gráfica via `Gloss`.

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
```
sudo apt-get update
sudo apt-get install swi-prolog
```
Para distribuições baseadas em Fedora:
```
sudo dnf install swi-prolog
```
Dica: Após a instalação, feche e reabra o terminal para garantir que o comando swipl esteja disponível no seu PATH.

## 🔧 Compilação do Projeto

Com o SWI-Prolog instalado, você pode iniciar o jogo diretamente pelo terminal.

Navegue até o diretório onde o arquivo terminal-hero.pl está localizado.

Execute o seguinte comando para carregar o jogo:
```
swipl terminal-hero.pl
```
Após o ambiente do Prolog ser carregado com o arquivo do jogo, inicie o jogo digitando o comando abaixo:
```
play.
```

Agora você está pronto para jogar! Divirta-se.

---

## 🕹️ Controles (Exemplo)

> (Dependente de como o jogo foi implementado)

- Teclas: `A`, `S`, `J`, `K` para acertar as notas.
- Pontuação é mostrada no canto superior da interface.
- Notas caem de cima e devem ser pressionadas no tempo certo.

---

## 📚 Referências

- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal](https://www.haskell.org/cabal/)
- [Gloss](https://hackage.haskell.org/package/gloss)

---

### Docs com funcionalidades do projeto
https://docs.google.com/document/d/1ur6RbqMDHaa-zNUNkXIWdY6Q51y-UFhaHQI1seneUY8/edit?tab=t.0

