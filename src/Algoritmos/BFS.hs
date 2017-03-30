module Algoritmos.BFS(
    bfs
) where
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Estruturas.Grafo
import Estruturas.VetorRoteamento
import Estruturas.ListaAdjacencia
import qualified Estruturas.Distancia as D
import qualified Estruturas.Pai as P

type Fim = Bool

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
          vs= Set.toList $ vertices g

passo :: Estado -> (Fim, Estado)
passo e@(Estado _ [] _)       = (True , e)
passo e@(Estado ls (q:qs) vs) = (False, ks { vetor = os })
    where as = ls Map.! q
          p = P.Pai q
          ks = Set.foldl (visitar p) e { fila = qs } as
          os = Map.adjust (\x-> x { descoberta = Completo }) q $ vetor ks

loop :: Estado -> Estado
loop estado = if fim then novoEstado else loop novoEstado
    where (fim, novoEstado) = passo estado

visitar :: Pai -> Estado -> Vertice -> Estado
visitar p (Estado ls qs vr) v = Estado ls novaFila $ Map.adjust f v vr
    where novo = desconhecido $ descoberta $ vr Map.! v
          novoItem = ItemVetor d p Visitado 
          novaFila = if novo then qs ++ [v] else qs
          d = incDist $ distPai p vr
          f = \x -> if novo then novoItem else x

bfs :: Vertice -> Grafo -> VetorRoteamento
bfs v g = vetor . loop $ estadoInicial g v