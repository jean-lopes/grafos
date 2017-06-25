module Kruskal
where
import Data.List (sort, delete, nub)

data Edge a = Edge
    { origin :: a
    , destiny :: a
    , cost :: Int
    } deriving (Show, Eq)

data Graph a = Graph 
    { vertices :: [a] 
    , edges :: [Edge a]
    } deriving Show

newtype Set a = Set [a]
    deriving (Show, Eq)

instance Ord a => Ord (Edge a) where
    compare (Edge u1 v1 c1) (Edge u2 v2 c2) = 
        let u = compare u1 u2
            c = compare c1 c2
        in case c of
            EQ -> case u of
                EQ -> compare v1 v2
                _  -> u
            _  -> c               

make :: a -> Set a
make v = Set [v]

match :: Eq a => a -> Set a -> Bool
match x (Set ys) = elem x ys

find :: Eq a => a -> [Set a] -> Set a
find x [] = make x
find x (set:sets)
    | match x set = set
    | otherwise = find x sets

union :: Ord a => Set a -> Set a -> [Set a] -> [Set a]
union x@(Set xs) y@(Set ys) zs = set:sets
    where sets = delete x $ delete y zs
          set = Set . nub . sort $ xs ++ ys

kruskal :: Ord a => Graph a -> (Int, [Edge a])
kruskal (Graph vs es) = (totalCost, tree)
    where sets = map make vs
          edges = sort es
          tree = sort . snd $ foldl step (sets, []) edges
          totalCost = sum $ map cost tree
          
step :: Ord a => ([Set a], [Edge a]) -> Edge a -> ([Set a], [Edge a])
step (sets, edges) edge@(Edge u v _)
    | us == vs = (sets, edges)
    | otherwise = (union us vs sets, edge:edges)    
    where 
        us = find u sets
        vs = find v sets

g1 = Graph
    { vertices = "ABCDEF"
    , edges = 
        [ Edge 'A' 'B' 4
        , Edge 'A' 'E' 4
        , Edge 'A' 'D' 3
        , Edge 'B' 'E' 9
        , Edge 'B' 'F' 5
        , Edge 'B' 'C' 8
        , Edge 'C' 'F' 2
        , Edge 'C' 'D' 9
        , Edge 'C' 'E' 3
        , Edge 'D' 'F' 7
        , Edge 'E' 'F' 2
        ]
    }

g2 = Graph
    { vertices = "ABCDEFGHIJLM"
    , edges = 
        [ Edge 'A' 'B' 4
        , Edge 'A' 'M' 7
        , Edge 'A' 'I' 4
        , Edge 'B' 'C' 4
        , Edge 'B' 'L' 8
        , Edge 'C' 'D' 3
        , Edge 'C' 'M' 6
        , Edge 'D' 'E' 1
        , Edge 'D' 'L' 2
        , Edge 'E' 'F' 3
        , Edge 'F' 'J' 2
        , Edge 'J' 'L' 4
        , Edge 'J' 'G' 2
        , Edge 'G' 'M' 5
        , Edge 'H' 'I' 3
        , Edge 'I' 'J' 6
        , Edge 'H' 'G' 7
        ]
    }