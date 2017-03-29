module Estruturas.ListaAdjacencia(
    ListaAdjacencia,
    listaAdjacencia
) where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Estruturas.Grafo

type ListaAdjacencia = Map.Map Vertice Vertices

type FuncAdjacentes = Vertice -> Aresta -> [Vertice]

listaAdjacencia :: Grafo -> ListaAdjacencia
listaAdjacencia g@(Grafo   _ _) = Map.fromList $ mapAdjacentes g funcAdjacentesGrafos
listaAdjacencia g@(Digrafo _ _) = Map.fromList $ mapAdjacentes g funcAdjacentesDigrafos

filtrarArestas :: Vertice -> Arestas -> Arestas
filtrarArestas x es = Map.filter f es
    where f = \e -> x == origem e || x == destino e

adjacente :: (Aresta -> [Vertice]) -> Aresta -> [Vertice]
adjacente f e = if loop then [origem e] else f e
    where loop = origem e == destino e

adjacentes :: Vertice -> FuncAdjacentes -> Arestas -> [Vertice]
adjacentes x f es = List.nub $ List.sort $ List.concatMap (adjacente (f x)) fes
    where fes = Map.elems $ filtrarArestas x es

funcAdjacentesGrafos :: FuncAdjacentes
funcAdjacentesGrafos x e = List.filter (/=x) [origem e, destino e]

funcAdjacentesDigrafos :: FuncAdjacentes
funcAdjacentesDigrafos x e = if x == origem e then [destino e] else []

mapAdjacentes :: Grafo -> FuncAdjacentes -> [(Vertice, Vertices)]
mapAdjacentes g f = map (\x -> (x, adjs x)) $ Set.elems vs
    where vs   = vertices g
          es   = arestas g
          adjs = \x -> Set.fromList $ adjacentes x f es