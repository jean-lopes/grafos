module Algoritmos.BFS(
    bfs
) where
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Queue as Queue
import Estruturas.Grafo
import Estruturas.VetorRoteamento
import Estruturas.ListaAdjacencia
import qualified Estruturas.Distancia as D
import qualified Estruturas.Pai as P

type Fim = Bool

-- Fila usando Listas
pop :: [a] -> Maybe (a, [a])
pop []     = Nothing
pop (x:xs) = Just (x, xs)

push :: a -> [a] -> [a]
push x xs = xs ++ [x]

data Estado = Estado { adjacentes :: ListaAdjacencia 
                     , fila       :: [Vertice]
                     , vetor      :: VetorRoteamento
                     } deriving (Show, Eq, Ord)

estadoInicial :: Grafo -> Vertice -> Estado
estadoInicial g x = Estado { adjacentes = xs, fila = [x], vetor = vs }
    where xs = listaAdjacencia g
          vs = Map.adjust initItem x $ inicializar g
          initItem = \iv -> iv { distancia = D.Valor 0
                               , descoberta = Visitado }

inicializar :: Grafo -> VetorRoteamento
inicializar g = Map.fromList $ zip vs itens
    where createItem = \_ -> ItemVetor D.Infinito P.Nil Desconhecido
          itens = map createItem vs 
          vs = Set.toList $ vertices g

bfs :: Vertice -> Grafo -> VetorRoteamento
bfs v g = Map.fromList []
    where vr = inicializar g
          s = [v]

passo :: Estado -> (Fim, Estado)
passo (Estado ls [] vs)     = (True , Estado ls [] vs)
passo (Estado ls (q:qs) vs) = (False, Estado ls qs vs)
    where as = ls Map.! q
          v = vs Map.! q

visitar :: Pai -> (Vertice, ItemVetor) -> VetorRoteamento -> VetorRoteamento
visitar p (v, iv) vr = adjust vr
    where (ItemVetor dist p desc) = vr Map.! v
          novaDist = incDist $ distPai p vr
          novoItem = if desc == Desconhecido
                        then ItemVetor novaDist p Visitado 
                        else iv
}


