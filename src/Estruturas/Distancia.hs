module Estruturas.Distancia
where

data Distancia = Infinito
               | Valor Int
               deriving (Show, Eq)

instance Ord Distancia where
    Infinito  `compare` Infinito  = EQ
    Infinito  `compare` (Valor _) = GT
    (Valor _) `compare` Infinito  = LT
    (Valor x) `compare` (Valor y) = compare x y

incDist :: Distancia -> Distancia
incDist Infinito  = Infinito
incDist (Valor x) = Valor $ x + 1