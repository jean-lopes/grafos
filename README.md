O algoritmo de Kruskal é um algoritmo em teoria dos grafos que busca uma árvore geradora mínima para um grafo conexo com pesos. Isto significa que ele encontra um subconjunto das arestas que forma uma árvore que inclui todos os vértices, onde o peso total, dado pela soma dos pesos das arestas da árvore, é minimizado. Se o grafo não for conexo, então ele encontra uma floresta geradora mínima (uma árvore geradora mínima para cada componente conexo do grafo). O algoritmo de Kruskal é um exemplo de um algoritmo guloso (também conhecido como ganancioso ou greedy).

Seu funcionamento é mostrado a seguir:

crie uma floresta F (um conjunto de árvores), onde cada vértice no grafo é uma árvore separada
crie um conjunto S contendo todas as arestas do grafo
enquanto S for não-vazio, faça:
remova uma aresta com peso mínimo de S
se essa aresta conecta duas árvores diferentes, adicione-a à floresta, combinando duas árvores numa única árvore parcial
do contrário, descarte a aresta
Ao fim do algoritmo, a floresta tem apenas um componente e forma uma árvore geradora mínima do grafo.

Com o uso de uma estrutura de dados eficiente, o algoritmo de Kruskal possui complexidade de tempo igual a O (m log n), onde m representa o número de arestas e n o número de vértices.
O algoritmo de Kruskal é um algoritmo em teoria dos grafos que busca uma árvore geradora mínima para um grafo conexo com pesos. Isto significa que ele encontra um subconjunto das arestas que forma uma árvore que inclui todos os vértices, onde o peso total, dado pela soma dos pesos das arestas da árvore, é minimizado. Se o grafo não for conexo, então ele encontra uma floresta geradora mínima (uma árvore geradora mínima para cada componente conexo do grafo). O algoritmo de Kruskal é um exemplo de um algoritmo guloso (também conhecido como ganancioso ou greedy).

Seu funcionamento é mostrado a seguir:

crie uma floresta F (um conjunto de árvores), onde cada vértice no grafo é uma árvore separada
crie um conjunto S contendo todas as arestas do grafo
enquanto S for não-vazio, faça:
remova uma aresta com peso mínimo de S
se essa aresta conecta duas árvores diferentes, adicione-a à floresta, combinando duas árvores numa única árvore parcial
do contrário, descarte a aresta
Ao fim do algoritmo, a floresta tem apenas um componente e forma uma árvore geradora mínima do grafo.

Com o uso de uma estrutura de dados eficiente, o algoritmo de Kruskal possui complexidade de tempo igual a O (m log n), onde m representa o número de arestas e n o número de vértices.
