module Estruturas.Pai
where
import Estruturas.Vertice

data Pai = Nil
         | Pai Vertice
         deriving (Show, Eq)

instance Ord Pai where
    Nil     `compare` Nil     = EQ
    Nil     `compare` (Pai _) = LT
    (Pai _) `compare` Nil     = GT
    (Pai x) `compare` (Pai y) = x `compare` y

temPai :: Pai -> Bool
temPai Nil = False
temPai _   = True