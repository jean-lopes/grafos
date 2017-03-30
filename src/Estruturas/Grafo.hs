module Estruturas.Grafo(
    Grafo(..), g1,
    module Estruturas.Vertice,
    module Estruturas.Aresta,
) where
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Estruturas.Aresta
import Estruturas.Vertice

data Grafo = Grafo   { vertices :: Vertices, arestas :: Arestas }
           | Digrafo { vertices :: Vertices, arestas :: Arestas }
           deriving (Show, Eq, Ord)

g1 = Grafo { vertices = Set.fromList [ "r", "s", "t", "u", "v", "w", "x", "y" ]
           , arestas  = Map.fromList [ ( "e1", Aresta "r" "s" SemPeso)
                                     , ( "e2", Aresta "r" "v" SemPeso)
                                     , ( "e3", Aresta "s" "w" SemPeso)
                                     , ( "e4", Aresta "w" "t" SemPeso)
                                     , ( "e5", Aresta "w" "x" SemPeso)
                                     , ( "e6", Aresta "t" "x" SemPeso)
                                     , ( "e7", Aresta "t" "u" SemPeso)
                                     , ( "e8", Aresta "x" "u" SemPeso)
                                     , ( "e9", Aresta "u" "y" SemPeso)
                                     , ("e10", Aresta "x" "y" SemPeso) ] }