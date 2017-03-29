module Estruturas.Complexidade
where
    
data Complexidade = Simples
                  | Multigrafo
                  deriving (Show, Eq)

instance Ord Complexidade where
    Simples    `compare` Simples    = EQ
    Simples    `compare` Multigrafo = LT
    Multigrafo `compare` Simples    = GT
    Multigrafo `compare` Multigrafo = EQ
