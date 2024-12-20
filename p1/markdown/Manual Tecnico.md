<header>
<style>
OL { counter-reset: item }
.linha { display: block }
.linha:before { content: counters(item, ".") ". "; counter-increment: item }
</style>
</header>

# <center>Manual Técnico</center>

### <center>Inteligência Artificial - Escola Superior de Tecnologia de Setúbal</center>

<center>2024/2025</center>

<br />

### <center>Prof. Joaquim Filipe</center>
### <center>Eng. Filipe Mariano</center>

<br />

### <center>202000634 Bruno Ascenção</center>
### <center>202000584 Francisco Pereira</center>

<br />

### <center>Projeto Nº 1: Época Normal</center>
<center>20/12/2024</center>

<br/>
<hr/>

# Índice
<ol>
<li class="linha">Arquitetura do sistema</li>
<ol>
<li class="linha">puzzle.lisp</li>
<li class="linha">search.lisp</li>
<li class="linha">branching.lisp</li>
<li class="linha">project.lisp</li>
<li class="linha">Conexões e relacionamentos</li>
</ol>
<li class="linha">Entidades e sua implementação</li>
<ol>
<li class="linha">Nós</li>
</ol>
<li class="linha">Algoritmos e sua implementação</li>
<li class="linha">Descrição das opções tomadas</li>
<ol>
<li class="linha">Dependências</li>
</ol>
<li class="linha">Limitações técnicas e desenvolvimento futuro</li>
<ol>
<li class="linha">Limitações técnicas</li>
<li class="linha">Desenvolvimento futuro</li>
</ol>
<li class="linha">Testes e resultados</li>
<ol>
<li class="linha">BFS</li>
<li class="linha">A*</li>
<li class="linha">DFS</li>
</ol>
</ol>

<br/>
<hr/>

# 1. Arquitetura do sistema
<p>O projeto contém 4 módulos, estão abaixo descritos quais são e para que servem:</p>

## 1.1. puzzle.lisp
<p>Este módulo contém tudo o que está relacionado com o problema como por exemplo, os operadores, getters e setters.</p>

## 1.2. search.lisp
<p>Este módulo contém toda a lógica dos algoritmos, BFS, DFS e A*.</p>

## 1.3. branching.lisp
<p>Este módulo contém a lógica relacionada com o cálculo da bissecção.</p>

## 1.4. project.lisp
<p>Este módulo é o cérebro do projeto. Utiliza todos os outros módulos para resolver o problema.</p>

## 1.5. Conexões e relacionamentos
<p>Como referido anteriormente o project.lisp faz a gestão de todos os módulos atuando como o cérebro do programa. Isso significa que não existe uma ligação entre os módulos, existe sim uma ligação geral de todos os módulos com o project.lisp, sendo assim este módulo desempenha o papel de mediador entre os restantes.</p>
<p>Seguindo a lógica de execução do programa, são realizadas as seguintes trocas de informação:</p>
<ul>
<li>project utiliza o puzzle para criar nó inicial consoante o problema escolhido pelo utilizador</li>
<li>project envia para o search o nó, as dependências e qual o algoritmo a ser executado</li>
<li>search executa o algoritmo e devolve ao project os resultados obtidos</li>
<li>project executa o branching para o cálculo da ramificação média, utilizando os dados recebidos do search</li>
<li>branching devolve o cálculo da ramificação média ao project</li>
</ul>

<br/>
<hr/>

# 2. Entidades e sua implementação
## 2.1. Nós
<p>Os nós estão estruturados através de uma lista com quatro elementos:</p>
<ul>
<li>Estado;</li>
<li>Profundidade;</li>
<li>Valor heurístico;</li>
<li>Nó pai.</li>
</ul>

```lisp
(((7 1 7 7 7 0) (1 16 15 1 11 2)) 28 0 ((6 0 6 6 6 8) (0 15 14 1 11 2)) 27 0 nil)
```

<br/>
<hr/>

# 3. Algoritmos e sua implementação
<p>Neste projeto foram implementados três algoritmos que são os seguintes</p>

## 3.1. BFS
<p>O bfs, breadth first search, é um algoritmo de pesquisa em largura. Isto é, apenas avança para o nível seguinte após gerar todos os sucessores do nível atual. Enquanto são gerados os novos nós, o bfs vai garantir que não irá gerar nós já existentes, evitando assim loops infinitos.</p>
<p>A validação do nó solução é feita na geração dos sucessores, ou seja, quando são criados os novos nós é realizada ao mesmo tempo a validação se algum desses nós é a solução do problema.</p>

```lisp
(DEFUN bf(dependencies opened-list &optional (closed-list NIL))
  "Method to execute the breath-first algorithm"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print to see if algorithhm is working
      (LET*
        (
          (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list)) ;Generate all successors
          (solution (solution-from-successors dependencies valid-successors)) ;Get the solution if there is
        )
        (IF solution ;Verify if solution
          ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
          (return-data solution (APPEND (CDR opened-list) valid-successors) (CONS (CAR opened-list) closed-list)) ;Return solution and the data
          (bf dependencies (APPEND (CDR opened-list) valid-successors) (CONS (CAR opened-list) closed-list));Go to next iteration
        )
      )
    )
  )
)
```

## 3.2. DFS
<p>O dfs, depth first search, contrariamente ao bfs é um algoritmo de procura em profundidade. Isto é, o algoritmo gera sempre primeiro os sucessores do primeiro sucessor do nó atual. Por sem um algoritmo em profundidade, é necessário indicar o nível máximo a atingir com o objetivo de evitar a geração contínua de nós sucessores.</p>
<p>De forma igual ao bfs, neste algoritmo é necessário garantir que não é permitido gerar nós já existentes, para que mais uma vez, não existam loops infinitos.</p>
<p>Também como o bfs, se algum nó sucessor gerado for a solução, o mesmo será retornado.</p>

```lisp
(DEFUN df(dependencies max-depth opened-list &optional (closed-list NIL))
  "Method to execute the depth-first algorithm"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print to see if algorithhm is working
      ;Validate the current node depth
      (IF (>= (FUNCALL (dependency-depth dependencies) (CAR opened-list)) max-depth) ;Current node depth is higher than the max allowed depth
        (df dependencies max-depth (CDR opened-list) (CONS (CAR opened-list) closed-list)) ;Go to next iteration without spawning successors
        ;Max depth not reach so keep executing
        (LET*
          (
            (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list)) ;Generate all successors
            (solution (solution-from-successors dependencies valid-successors)) ;Get the solution if there is
          )
          (IF solution ;Verify if solution
            ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
            (return-data solution (APPEND valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list)) ;Return solution and the data
            (df dependencies max-depth (APPEND valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list));Go to next iteration
          )
        )
      )
    )
  )
)
```

## 3.3. A*
<p>O A*, contrariamente aos algoritmos já antes falados, avalia os nós utilizando uma heurística. Esta heurística serve para que o algoritmo ao realizar a pesquisa, ordene os sucessores de forma crescente consoante o valor heurístico. Isto permite que o A* chegue de forma mais rápida ao nó solução devido à ordenação dos sucessores, fazendo assim com que ao tentar expandir um nó e este tenha o valor heurístico 0, está subentendido que este será o nó solução.</p>
<p>Igualmente aos outros dois algoritmos já falados, este também garante que não será gerado nenhum nó sucessor já existente, evitando os loops infinitos.</p>

```lisp
(DEFUN a*(dependencies heuristic opened-list &optional (closed-list NIL))
  "Method to execute the a* algorithm, recieving the puzzle dependencies the heuristicm method and the first node as a list on the opened list"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print working node
      ;Validate if current node is the solution
      (IF (FUNCALL (dependency-is-solution dependencies) (CAR opened-list)) ;Verify if node to be worked is solution
        (return-data (CAR opened-list) opened-list closed-list) ;Node is the solution return data~
        ;Node not solution so keep executing
        (LET
          (
            (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list heuristic)) ;Generate all successors
          )
          (a* dependencies heuristic (a*_add-to-open dependencies valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list)) ;Go to next iteration
        )
      )
    )
  )
)
```

<br/>
<hr/>

# 4. Descrição das opções tomadas
## 4.1. Dependências
<p>As dependências é uma lista de métodos pertencentes ao módulo do puzzle, estes métodos simulam métodos dentro dos obejtos na progamação orientada a objetos. Isto permite que os algoritmos tenham acesso indireto aos getters e setters dos nós, sem estarem diretamente ligados com o módulo puzzle.</p>

<br/>
<hr/>

# 5. Limitações técnicas e desenvolvimento futuro
## 5.1. Limitações técnicas
<ul>
<li>DFS - Não existe o recálculo da profundidade, no caso de haver um sucessor já fechado mas tem menor profundidade;</li>
<li>A* - Não existe o recálculo da heurística quando é encontrado um sucessor já fechado mas com menor valor heurístico;</li>
<li>Algoritmos extras não foram realizados;</li>
<li>Acreditamos que a heurística desenvolvida não seja a mais eficiente, contudo é funcional;</li>
<li>Devido ao funcionamento do BFS e DFS, muitos dos problemas dados no enunciado do projeto não puderam ser realizados já que o LispWorks é limitado na memória e em alguns problemas essa memória esgota-se antes do algoritmo terminar a resolução do problema.</li>
</ul>

## 5.2. Desenvolvimento futuro
<ul>
<li>Algoritmos extras pedidos no enunciado;</li>
<li>Refactoring do código para melhor análise;</li>
<li>Utilização de closures para um uso mais eficiente da memória e assim possibilitar a que os algoritmos BFS e DFS possam talvez finalizar os problemas que agora não conseguem;</li>
<li>Corrigir os problemas mencionados nas limitações técnicas.</li>
</ul>

<br/>
<hr/>

# 6. Testes e resultados
<p>Todos os dados inseridos estão guardados na pasta "statistics" em documentos de texto. Estes são gerados pelo programa no final da execução do algoritmo.</p>
<p>ABF - Average Branching Factor</p>

## 6.1. BFS
<table>
<tr>
<th>Problema</th>
<th>Nós gerados</th>
<th>Nós expandidos</th>
<th>g(x) Profundidade</th>
<th>Penetrância</th>
<th>ABF</th>
<th>Tempo(s)</th>
</tr>
<tr>
<td>((0 0 0 0 0 2) (0 0 0 0 4 0))</td>
<td>22</td>
<td>10</td>
<td>4</td>
<td>0.18181819</td>
<td>1.8208008</td>
<td>0.0</td>
</tr>
<tr>
<td>((2 2 2 2 2 2) (2 2 2 2 2 2))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((0 3 0 3 0 3) (3 0 3 0 3 0))</td>
<td>2267</td>
<td>626</td>
<td>6</td>
<td>0.0026466695</td>
<td>3.415928</td>
<td>0.142</td>
</tr>
<tr>
<td>((1 2 3 4 5 6) (6 5 4 3 2 1))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((2 4 6 8 10 12) (12 10 8 6 4 2))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((48 0 0 0 0 0) (0 0 0 0 0 48))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((8 8 8 8 8 8) (8 8 8 8 8 8))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
</table>

<br/>
<hr/>

## 6.2. A*
<table>
<tr>
<th>Problema</th>
<th>Heurística</th>
<th>Nós gerados</th>
<th>Nós expandidos</th>
<th>g(x) Profundidade</th>
<th>Penetrância</th>
<th>ABF</th>
<th>Tempo(s)</th>
</tr>
<tr>
<td>((0 0 0 0 0 2) (0 0 0 0 4 0))</td>
<td>Default</td>
<td>12</td>
<td>5</td>
<td>6</td>
<td>0.5</td>
<td>1.2011719</td>
<td>0.001</td>
</tr>
<tr>
<td>((0 0 0 0 0 2) (0 0 0 0 4 0))</td>
<td>Custom</td>
<td>12</td>
<td>5</td>
<td>6</td>
<td>0.5</td>
<td>1.2011719</td>
<td>0.001</td>
</tr>
<tr>
<td>((2 2 2 2 2 2) (2 2 2 2 2 2))</td>
<td>Default</td>
<td>78</td>
<td>13</td>
<td>14</td>
<td>0.17948719</td>
<td>1.2092285</td>
<td>0.003</td>
</tr>
<tr>
<td>((2 2 2 2 2 2) (2 2 2 2 2 2))</td>
<td>Custom</td>
<td>69</td>
<td>12</td>
<td>12</td>
<td>0.17391305</td>
<td>1.2550049</td>
<td>0.003</td>
</tr>
<tr>
<td>((0 3 0 3 0 3) (3 0 3 0 3 0))</td>
<td>Default</td>
<td>39</td>
<td>9</td>
<td>10</td>
<td>0.25641027</td>
<td>1.2473145</td>
<td>0.002</td>
</tr>
<tr>
<td>((0 3 0 3 0 3) (3 0 3 0 3 0))</td>
<td>Custom</td>
<td>35</td>
<td>9</td>
<td>9</td>
<td>0.25714287</td>
<td>1.2731934</td>
<td>0.002</td>
</tr>
<tr>
<td>((1 2 3 4 5 6) (6 5 4 3 2 1))</td>
<td>Default</td>
<td>274</td>
<td>42</td>
<td>35</td>
<td>0.12773723</td>
<td>1.095398</td>
<td>0.022</td>
</tr>
<tr>
<td>((1 2 3 4 5 6) (6 5 4 3 2 1))</td>
<td>Custom</td>
<td>359</td>
<td>93</td>
<td>29</td>
<td>0.08077995</td>
<td>1.1448822</td>
<td>0.028</td>
</tr>
<tr>
<td>((2 4 6 8 10 12) (12 10 8 6 4 2))</td>
<td>Default</td>
<td>282</td>
<td>40</td>
<td>41</td>
<td>0.14539007</td>
<td>1.0757446</td>
<td>0.013</td>
</tr>
<tr>
<td>((2 4 6 8 10 12) (12 10 8 6 4 2))</td>
<td>Custom</td>
<td>704</td>
<td>238</td>
<td>46</td>
<td>0.06534091</td>
<td>1.090332</td>
<td>0.085</td>
</tr>
<tr>
<td>((48 0 0 0 0 0) (0 0 0 0 0 48))</td>
<td>Default</td>
<td>616</td>
<td>163</td>
<td>45</td>
<td>0.073051945</td>
<td>1.0997315</td>
<td>0.125</td>
</tr>
<tr>
<td>((48 0 0 0 0 0) (0 0 0 0 0 48))</td>
<td>Custom</td>
<td>453</td>
<td>117</td>
<td>46</td>
<td>0.10154525</td>
<td>1.0852203</td>
<td>0.035</td>
</tr>
<tr>
<td>((8 8 8 8 8 8) (8 8 8 8 8 8))</td>
<td>Default</td>
<td>336</td>
<td>47</td>
<td>46</td>
<td>0.13690476</td>
<td>1.0715332</td>
<td>0.017</td>
</tr>
<tr>
<td>((8 8 8 8 8 8) (8 8 8 8 8 8))</td>
<td>Custom</td>
<td>422</td>
<td>86</td>
<td>49</td>
<td>0.116113745</td>
<td>1.0753479</td>
<td>0.026</td>
</tr>
</table>

<br/>
<hr/>

## 6.3. DFS
<table>
<tr>
<th>Problema</th>
<th>Nós gerados</th>
<th>Nós expandidos</th>
<th>g(x) Profundidade</th>
<th>Profundidade máxima</th>
<th>Penetrância</th>
<th>ABF</th>
<th>Tempo(s)</th>
</tr>
<tr>
<td>((0 0 0 0 0 2) (0 0 0 0 4 0))</td>
<td>11</td>
<td>5</td>
<td>6</td>
<td>28</td>
<td>0.54545457</td>
<td>1.1762695</td>
<td>0.001</td>
</tr>
<tr>
<td>((2 2 2 2 2 2) (2 2 2 2 2 2))</td>
<td>95</td>
<td>17</td>
<td>18</td>
<td>28</td>
<td>0.18947369</td>
<td>1.1538696</td>
<td>0.003</td>
</tr>
<tr>
<td>((0 3 0 3 0 3) (3 0 3 0 3 0))</td>
<td>39</td>
<td>9</td>
<td>10</td>
<td>28</td>
<td>0.25641027</td>
<td>1.2473145</td>
<td>0.002</td>
</tr>
<tr>
<td>((1 2 3 4 5 6) (6 5 4 3 2 1))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((2 4 6 8 10 12) (12 10 8 6 4 2))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((48 0 0 0 0 0) (0 0 0 0 0 48))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
<tr>
<td>((8 8 8 8 8 8) (8 8 8 8 8 8))</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
<td>Na</td>
</tr>
</table>