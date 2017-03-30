module Estruturas.Descoberta(
    Descoberta(..),
    desconhecido,
    visitado,
    completo,
)where

data Descoberta = Desconhecido
                | Visitado
                | Completo
                deriving (Show, Eq)

instance Ord Descoberta where
    Desconhecido `compare` Desconhecido = EQ
    Desconhecido `compare` Visitado     = LT
    Desconhecido `compare` Completo     = LT

    Completo     `compare` Desconhecido = GT
    Completo     `compare` Visitado     = GT
    Completo     `compare` Completo     = EQ

    Visitado     `compare` Desconhecido = GT
    Visitado     `compare` Visitado     = EQ
    Visitado     `compare` Completo     = LT

desconhecido :: Descoberta -> Bool
desconhecido = (==Desconhecido)

visitado :: Descoberta -> Bool
visitado = (==Visitado)

completo :: Descoberta -> Bool
completo = (==Completo)