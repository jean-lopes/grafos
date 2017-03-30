module Estruturas.Peso(
    Peso(..)
) where

data Peso = SemPeso
          | Infinito 
          | Peso Int
          deriving (Show, Eq)

instance Ord Peso where
    SemPeso  `compare` SemPeso  = EQ
    SemPeso  `compare` _        = LT
    Infinito `compare` Infinito = EQ
    Infinito `compare` _        = GT
    (Peso x) `compare` (Peso y) = x `compare` y
    (Peso _) `compare` SemPeso  = GT
    (Peso _) `compare` Infinito = LT