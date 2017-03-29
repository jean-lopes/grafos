module Estruturas.Peso
where

data Peso = Nil
          | Infinito 
          | Valor Int
          deriving (Show, Eq)

instance Ord Peso where
    Nil       `compare` Nil       = EQ
    Nil       `compare` _         = LT
    Infinito  `compare` Infinito  = EQ
    Infinito  `compare` _         = GT
    (Valor x) `compare` (Valor y) = x `compare` y
    (Valor _) `compare` Nil       = GT
    (Valor _) `compare` Infinito  = LT