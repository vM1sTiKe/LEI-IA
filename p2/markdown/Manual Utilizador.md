<header>
<style>
OL { counter-reset: item }
LI { display: block }
LI:before { content: counters(item, ".") ". "; counter-increment: item }
</style>
</header>

# <center>Manual de Utilizador</center>

### <center>Inteligência Artificial - Escola Superior de Tecnologia de Setúbal</center>

<center>2024/2025</center>

<br />

### <center>Prof. Joaquim Filipe</center>
### <center>Eng. Filipe Mariano</center>

<br />

### <center>202000634 Bruno Ascenção</center>
### <center>202000584 Francisco Pereira</center>

<br />

### <center>Projeto Nº 2: Época Normal</center>
<center>24/01/2025</center>

<br/>
<hr/>

# Índice
<ol>
<li>Introdução</li>
<li>Utilização</li>
<li>Limitações</li>
<ol>
<li>Profundidade máxima</li>
</ol>
</ol>

<br/>
<hr/>

# 1. Introdução
<p>O programa consiste no jogo “Adji-boto*” onde é possível ao utilizador jogar contra outro utilizador, contra uma IA, ou colocar duas IAs a jogarem uma contra a outra.</p>
<p></p>

<br/>
<hr/>

# 2. Utilização
<p>Ao compilar o programa, o primeiro comando a executar é o "(play)".</p>
<img src="./Imagens/M1.png">
<p>De seguida, o utilizador tem de escolher o modo de jogo utilizando um numéro entre 1 e 3, sendo as hipóteses "Player vs AI", "AI vs AI", "Player vs Player" respetivamente.</p>
<img src="./Imagens/M2.png">
<p>Ao selecionar a opção 1 "Player vs AI", o utilizador deve escolher quem começa a jogar, ele ou a IA. Pode escolher a opção através dos númeors 1 para o utilizador, ou 2 para a IA.</p>
<p>Já tendo escolhido quem começa a jogar, o utilizador deve decidir se quer utilizar a memoization escolhendo entre os números 1 para sim ou 2 para não.</p>
<img src="./Imagens/M3.png">
<p>Após a decisão sobre o uso do memoization, o jogo começa. Para cada jogada o utilizador deve indicar qual a coluna escolhida para a jogada, entre 1 e 6.</p>
<img src="./Imagens/M4.png">
<p>Após cada jogada do utilizador são exibidas as estatísticas da jogada da IA e repete o ciclo tendo o utilizador de inserir o número de 1 a 6.</p>
<img src="./Imagens/M5.png">
<p>Quando o modo de jogo é IA vs IA, o utilizador apenas tem de observar e esperar o fim do jogo. Se o modo de jogo for utilizador vs utilizador, ambos devem jogar na sua vez. Sempre que o jogo terminar, o utilizador deverá inserir novamente o comando "(play)" se quiser jogar novamente.</p>

<br/>
<hr/>

# 3. Limitações

## 3.1. Profundidade máxima
<p>Não é permitido ao utilizador indicar a profundidade máxima que o algoritmo irá utilizar como limite de procura.</p>