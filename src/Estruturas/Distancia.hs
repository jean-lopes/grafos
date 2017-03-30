module Estruturas.Distancia(
    Distancia(..),
    distString,
    incDist
)where

data Distancia = Infinito
               | Distancia Int
               deriving (Show, Eq)

instance Ord Distancia where
    Infinito      `compare` Infinito  = EQ
    Infinito      `compare` (Distancia _) = GT
    (Distancia _) `compare` Infinito  = LT
    (Distancia x) `compare` (Distancia y) = compare x y

distString :: Distancia -> String
distString Infinito      = " "
distString (Distancia n) = show n

incDist :: Distancia -> Distancia
incDist Infinito      = Infinito
incDist (Distancia x) = Distancia $ x + 1