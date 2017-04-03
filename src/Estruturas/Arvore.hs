module Estruturas.Arvore
where
import Estruturas.Vertice

data Arvore = Nodo { valor  :: Vertice
                   , filhos :: [Arvore]
                   } deriving (Show, Eq, Ord)

singleton :: Vertice -> Arvore
singleton x = Nodo x []


adicionarFilhos :: Arvore -> Vertice -> [Arvore] -> Arvore
adicionarFilhos a@(Nodo x fs) y zs = if x == y 
                                        then a { filhos = fs ++ zs }
                                        else if null fs 
                                                then a
                                                else adicionarFilhos a { filhos = tail fs } y zs

mostrarArvore :: Arvore -> IO ()
mostrarArvore = mostrarArvore' 0

mostrarArvore' :: Int -> Arvore -> IO ()
mostrarArvore' n (Nodo x fs) = do
        let ws = concat $ replicate (n*4) " "
            ln = ws ++ x
        putStrLn ln
        mapM_ (mostrarArvore' (n+1)) fs
        return ()
