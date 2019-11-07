{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module SDP.Tree where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

default ()

--------------------------------------------------------------------------------

-- | Tree - class of tree structures.
class (Ord e) => Tree t e | t -> e
  where
    {-# MINIMAL toTree, insertTree, deleteTree,
      (nodeElems|(elemCount,nodeElem)), (branches|(branchCount|branch)),
      ((/<|)|shiftTL), ((|>\)|shiftTR), ((</|)|shiftCTL), ((|\>)|shiftCTR) #-}
    
    {- Construction and deconstruction. -}
    
    -- | Creates tree from lists of elements and branches.
    toTree :: [e] -> [t] -> t
    
    -- | List of node elements.
    nodeElems :: t -> [e]
    nodeElems node = nodeElem node <$> [0 .. elemCount node - 1]
    
    -- | Returns n-th element of node.
    nodeElem :: t -> Int -> e
    nodeElem node = (nodeElems node !)
    
    -- | Returns count of elements of node.
    elemCount :: t -> Int
    elemCount =  length . nodeElems
    
    -- | List of node branches.
    branches :: t -> [t]
    branches node = branch node <$> [0 .. elemCount node - 1]
    
    -- | Returns n-th branch of node.
    branch :: t -> Int -> t
    branch node = (branches node !)
    
    branchCount :: t -> Int
    branchCount = length . branches
    
    {- Appending and shifting. -}
    
    -- | @(/+)@ appends pair of branch and element to left side of the tree.
    (/+) :: (e, t) -> t -> t
    (e, br) /+ node = toTree (e :> nodeElems node) (br :> branches node)
    
    -- | @(+\)@ appends pair of branch and element to right side of the tree.
    (+\) :: t -> (e, t) -> t
    node +\ (e, br) = toTree (nodeElems node :< e) (branches node :< br)
    
    -- | @br /* tree@ appends branch @br@ to left side of @tree@ node.
    (/*) :: t -> t -> t
    br /* node = toTree (nodeElems node) (br :> branches node)
    
    -- | @br *\ tree@ appends branch @br@ ro right side of @tree@ node.
    (*\) :: t -> t -> t
    node *\ br = toTree (nodeElems node) (branches node :< br)
    
    -- | Left n-position noncyclic shift of branches and elements.
    (/<|) :: t -> Int -> t
    tree /<| n = n < 1 ? tree $ shiftTL tree /<| (n - 1)
    
    -- | Right n-position noncyclic shift of branches ans elements.
    (|>\) :: t -> Int -> t
    tree |>\ n = n < 1 ? tree $ shiftTR tree |>\ (n - 1)
    
    -- | Left n-position cyclic shift of branches and elements.
    (</|) :: t -> Int -> t
    tree </| n = n < 1 ? tree $ shiftCTL tree </| (n - 1)
    
    -- | Right n-position cyclic shift of branches and elements.
    (|\>) :: t -> Int -> t
    tree |\> n = n < 1 ? tree $ shiftCTR tree |\> (n - 1)
    
    -- | Left 1-position noncyclic shift of branches and elements.
    shiftTL :: t -> t
    shiftTL =  (/<| 1)
    
    -- | Right 1-position noncyclic shift of branches and elements.
    shiftTR :: t -> t
    shiftTR =  (|>\ 1)
    
    -- | Left 1-position cyclic shift of branches and elements.
    shiftCTL :: t -> t
    shiftCTL =  (</| 1)
    
    -- | Right 1-position cyclic shift of branches and elements.
    shiftCTR :: t -> t
    shiftCTR =  (|\> 1)
    
    {- Common operations. -}
    
    isLeaf :: t -> Bool
    isLeaf node = branchCount node == 0
    
    -- | Number of element in current node from 0.
    nodePos :: e -> t -> Maybe Int
    nodePos e node = (== e) .$ nodeElems node
    
    -- | inserts element to given tree.
    insertTree :: e -> t -> t
    
    -- | deletes element from given tree.
    deleteTree :: e -> t -> t
    
    -- | Minimal element of tree.
    default minTree :: (Linear t e) => t -> e
    minTree :: t -> e
    minTree =  head
    
    -- | Maximal element of tree.
    default maxTree :: (Linear t e) => t -> e
    maxTree :: t -> e
    maxTree =  last


