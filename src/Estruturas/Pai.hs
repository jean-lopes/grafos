module Estruturas.Pai(
    module Estruturas.Vertice,    
    Pai(..),
    paiString,
    temPai
)where
import Estruturas.Vertice

data Pai = Nil
         | Pai Vertice
         deriving (Show, Eq)

instance Ord Pai where
    Nil     `compare` Nil     = EQ
    Nil     `compare` (Pai _) = LT
    (Pai _) `compare` Nil     = GT
    (Pai x) `compare` (Pai y) = x `compare` y

paiString :: Pai -> String
paiString Nil     = "Nil"
paiString (Pai v) = v

temPai :: Pai -> Bool
temPai Nil = False
temPai _   = True
