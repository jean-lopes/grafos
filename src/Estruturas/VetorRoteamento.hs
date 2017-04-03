module Estruturas.VetorRoteamento(
    VetorRoteamento,
    ItemVetor(..),
    distPai,
    mostrar,
    caminhoPara,
    arvore
) where
import Data.List
import qualified Data.Map.Strict as Map
import Estruturas.Pai
import Estruturas.Distancia
import Estruturas.Descoberta
import Estruturas.Vertice
import Estruturas.Arvore

type VetorRoteamento = Map.Map Vertice ItemVetor

data ItemVetor = ItemVetor { distancia  :: Distancia
                           , pai        :: Pai
                           , descoberta :: Descoberta
                           } deriving (Show, Eq, Ord)

distPai :: Pai -> VetorRoteamento -> Distancia
distPai Nil     _  = Distancia 0
distPai (Pai v) vr = if d == Infinito then Distancia 0 else d
    where d = distancia $ vr Map.! v

caminhoPara :: VetorRoteamento -> Vertice -> String
caminhoPara vr v = unwords $ intersperse "->" ls
    where ls = caminhoPara' vr v []

caminhoPara' :: VetorRoteamento -> Vertice -> [Vertice] -> [Vertice]
caminhoPara' vr v cs = if b && p == Nil then v:cs else c
    where b = v `Map.member` vr
          i = vr Map.! v
          p = pai i
          c = caminhoPara' vr (f p) $ v:cs
          f Nil     = ""
          f (Pai x) = x

arvore :: VetorRoteamento -> [Arvore]
arvore vr = map (\v -> arvore' vr v $ singleton v) vs
    where vs = Map.keys $ Map.filter (\(ItemVetor _ p _) -> p == Nil) vr    

arvore' :: VetorRoteamento -> Vertice -> Arvore -> Arvore
arvore' vr v a = adicionarFilhos a v as
    where fs = Map.keys $ Map.filter (filho v) vr
          as = map (\f -> arvore' vr f $ singleton f) fs

filho :: Vertice -> ItemVetor -> Bool
filho _ (ItemVetor _ Nil     _) = False
filho v (ItemVetor _ (Pai x) _) = v == x

mostrar :: VetorRoteamento -> IO ()
mostrar vr = do mapM_ (mapM putStr) linhas
    where ls = Map.toList vr
          vs = [""] ++ map fst ls
          ds = ["Dist"] ++map (distString . distancia . snd) ls
          ps = ["Pai"] ++map (paiString . pai . snd) ls
          sz = maximum $ concat $ map (map length) [vs,ds,ps]
          padLeft :: String -> String
          padLeft = \xs -> xs ++ replicate (sz - length xs) ' '
          pipes :: [String] -> [String]
          pipes = \xs -> ["| "] ++ intersperse "| " xs ++ ["|"]
          plus = map (\xs -> map (\x -> if x == '|' then '+' else '-') xs)
          ln1 = pipes $ map padLeft vs
          ln2 = pipes $ map padLeft ds
          ln3 = pipes $ map padLeft ps
          ln = plus ln1
          linhas = intersperse ["\n"] [ln,ln1,ln,ln2,ln,ln3,ln,["\n"]]