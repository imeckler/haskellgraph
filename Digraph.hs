{-# LANGUAGE RecordWildCards #-}

module Digraph(
      Digraph
    , empty
    , addVertex
    , Key
    , addArc
    , successors
    , vertices
    , arcs
    , adjustVertex
    , valueAt
    , vmap
    , unionK
    ) where

import qualified Data.Map as M
import Control.Arrow (first, second)
import Control.Applicative hiding (empty)

data Digraph v e = Digraph {
    adjacencies :: M.Map Int (v, M.Map Int e),
    count :: Int
} deriving (Show)

data Key = Key {unKey :: Int}
    deriving (Eq)

instance Show Key where
    show = show . unKey

empty :: Digraph v e
empty = Digraph (M.fromList []) 0

addVertex :: v -> Digraph v e -> (Key, Digraph v e)
addVertex v (Digraph {..}) = (Key count
                           , Digraph (M.insert count (v, M.empty) adjacencies) (count + 1))

adjustVertex :: (v -> v) -> Key -> Digraph v e -> Digraph v e
adjustVertex f (Key k) (Digraph {..}) = Digraph (M.adjust (first f) k adjacencies) count

addArc :: Key -> Key -> e -> Digraph v e -> Digraph v e
addArc (Key k1) (Key k2) e (Digraph {..}) = Digraph newAdjacencies count
    where newAdjacencies = M.adjust (second $ M.insert k2 e) k1 adjacencies

addArcs :: [((Key, Key), e)] -> Digraph v e -> Digraph v e
addArcs as g = foldl (\h ((k1, k2), e) -> addArc k1 k2 e h) g as

successors :: Key -> Digraph v e -> Maybe [(Key, e)]
successors (Key k) (Digraph {..}) =
    map (first Key) . M.toList . snd <$> M.lookup k adjacencies

valueAt :: Key -> Digraph v e -> Maybe v
valueAt (Key k) (Digraph {..}) = fst <$> M.lookup k adjacencies

vertices :: Digraph v e -> [(Key, v)]
vertices (Digraph {..}) = map (\(k, (v, _)) -> (Key k, v)) $ M.toList adjacencies

arcs :: Digraph v e -> [((Key, Key), e)]
arcs (Digraph {..}) = [((Key k1, Key k2), e) | (k1, (_, as)) <- M.toList adjacencies, (k2, e) <- M.toList as]

vmap :: (v -> t) -> Digraph v e -> Digraph t e
vmap f (Digraph {..}) = Digraph (M.map (first f) adjacencies) count

emap :: (e -> d) -> Digraph v e -> Digraph v d
emap f (Digraph {..}) = Digraph (M.map (second $ M.map f) adjacencies) count

unionK' :: (Key, Digraph v e) -> (Key, Digraph v e) -> (Key, Key, Digraph v e)
unionK' (Key k1, g1) (Key k2, g2) = (Key k1, Key (k2 + n1), Digraph adjs (n1 + n2))
    where n1 = count g1
          n2 = count g2
          adjs2' = M.map (second $ M.mapKeysMonotonic (+ count g1))
                 $ M.mapKeysMonotonic (+ count g1) (adjacencies g2)
          adjs = M.union (adjacencies g1) adjs2'

unionK :: (Key, Digraph v e) -> (Key, Digraph v e) -> (Key, Key, Digraph v e)
unionK a@(_, g1) b@(_, g2) = if count g1 > count g2 then unionK' a b else unionK' b a

rootedBinTree :: Int -> (Key, Digraph Int Int)
rootedBinTree 0 = addVertex 0 empty
rootedBinTree n = (root, addArcs [((root, leftRoot), 1), ((root, rightRoot), 1)] t)
    where t'@(subRoot, subTree)      = rootedBinTree (n - 1)
          (leftRoot, rightRoot, t'') = unionK t' t'
          (root, t)                  = addVertex n t''
