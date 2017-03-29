module Estruturas.Aresta(
    Aresta(..),
    Peso(..),
    Arestas
) where
import qualified Data.Map.Strict as Map
import Estruturas.Peso
import Estruturas.Vertice

type Arestas = Map.Map String Aresta

data Aresta = Aresta { origem  :: Vertice
                     , destino :: Vertice 
                     , peso    :: Peso
                     } deriving (Show, Eq, Ord)