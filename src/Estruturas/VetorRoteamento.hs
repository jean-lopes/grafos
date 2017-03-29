module Estruturas.VetorRoteamento(
    module Estruturas.Distancia,
    module Estruturas.Pai,
    module Estruturas.Descoberta,
    VetorRoteamento,
    ItemVetor(..),
    distPai
) where
import qualified Data.Map.Strict as Map
import Estruturas.Pai
import Estruturas.Distancia
import Estruturas.Descoberta
import Estruturas.Vertice

type VetorRoteamento = Map.Map Vertice ItemVetor

data ItemVetor = ItemVetor { distancia  :: Distancia
                           , pai        :: Pai
                           , descoberta :: Descoberta
                           } deriving (Show, Eq, Ord)

distPai :: Pai -> VetorRoteamento -> Distancia
distPai Nil     _  = Valor 0
distPai (Pai v) vr = if d == Infinito then Valor 0 else d
    where d = distancia $ vr Map.! v