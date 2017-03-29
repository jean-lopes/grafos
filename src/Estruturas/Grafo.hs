module Estruturas.Grafo(
    Grafo(..),
    module Estruturas.Vertice,
    module Estruturas.Aresta,
    g1
) where
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Estruturas.Aresta
import Estruturas.Vertice

data Grafo = Grafo   { vertices :: Vertices, arestas :: Arestas }
           | Digrafo { vertices :: Vertices, arestas :: Arestas }
           deriving (Show, Eq, Ord)

g1 = Grafo { vertices = Set.fromList [ "r", "s", "t", "u", "v", "w", "x", "y" ]
           , arestas  = Map.fromList [ ( "e1", Aresta "r" "s" Nil)
                                     , ( "e2", Aresta "r" "v" Nil)
                                     , ( "e3", Aresta "s" "w" Nil)
                                     , ( "e4", Aresta "w" "t" Nil)
                                     , ( "e5", Aresta "w" "x" Nil)
                                     , ( "e6", Aresta "t" "x" Nil)
                                     , ( "e7", Aresta "t" "u" Nil)
                                     , ( "e8", Aresta "x" "u" Nil)
                                     , ( "e9", Aresta "u" "y" Nil)
                                     , ("e10", Aresta "x" "y" Nil) ] }