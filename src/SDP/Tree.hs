{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module SDP.Tree
(
  -- * Common tree operations
  Tree (..),
  
  -- * Advanced tree operations
  ShiftTree (..), AppendTree (..), MultiTree
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

default ()

--------------------------------------------------------------------------------

{- |
  Tree - class of tree structures.
  
  Note that all functions except fixTree and cyclic shifts must be safe.
  Function toTree can only be unsafe if it uses 'fixTree'.
-}
class (Ord e) => Tree t e | t -> e
  where
    {-# MINIMAL (nodeElems|(elemCount,nodeElem)), (branches|(branchCount|branch)),
      toTree, fixTree, insertTree, deleteTree, ((</|)|shiftCTL), ((|\>)|shiftCTR) #-}
    
    {- Construction and deconstruction. -}
    
    {- |
      Creates tree from lists of elements and branches, should be defined
      everywhere.
      * If there is not enough data, it returns an empty tree.
      * If there is too much data, ignores them.
      * If the branches or the resulting tree is incorrect, calls 'fixTree' as
      the last chance.
    -}
    toTree :: [e] -> [t] -> t
    
    -- | Tree normalization function, may fail.
    fixTree :: t -> t
    
    -- | List of node elements.
    nodeElems :: t -> [e]
    nodeElems node = nodeElem node <$> [0 .. elemCount node - 1]
    
    -- | Returns n-th element of node (from 0), may fail.
    nodeElem :: t -> Int -> e
    nodeElem node = (nodeElems node !)
    
    -- | Returns count of node elements, nonnegative.
    elemCount :: t -> Int
    elemCount =  length . nodeElems
    
    -- | List of node branches.
    branches :: t -> [t]
    branches node = branch node <$> [0 .. elemCount node - 1]
    
    -- | Returns n-th branch of node (from 0), may fail.
    branch :: t -> Int -> t
    branch node = (branches node !)
    
    -- | Returns count of branches, nonnegative.
    branchCount :: t -> Int
    branchCount = length . branches
    
    {- Common operations. -}
    
    isLeaf :: t -> Bool
    isLeaf node = branchCount node == 0
    
    -- | Number of element in current node from 0.
    nodePos :: e -> t -> Maybe Int
    nodePos e node = (== e) .$ nodeElems node
    
    {- |
      Inserts element to tree. If the tree already contains such an element and
      duplicates are not allowed, it should return the given (or equivalent)
      tree.
    -}
    insertTree :: e -> t -> t
    
    {- |
      Deletes element from tree. If the tree doesn't contain such an element, it
      should return the given (or equivalent) tree.
      
      deleteTree doesn't guarantee that the tree will not contain any such
      elements - if the data is incorrect, it may not be found. Also, some trees
      allow duplicates.
    -}
    deleteTree :: e -> t -> t
    
    -- | Minimal element of tree, may fail.
    default minTree :: (Linear t e) => t -> e
    minTree :: t -> e
    minTree =  head
    
    -- | Maximal element of tree, may fail.
    default maxTree :: (Linear t e) => t -> e
    maxTree :: t -> e
    maxTree =  last
    
    {- Cyclic shifts. -}
    
    -- | Left n-position cyclic shift of branches and elements.
    (</|) :: t -> Int -> t
    tree </| n = n < 1 ? tree $ shiftCTL tree </| (n - 1)
    
    -- | Right n-position cyclic shift of branches and elements.
    (|\>) :: t -> Int -> t
    tree |\> n = n < 1 ? tree $ shiftCTR tree |\> (n - 1)
    
    -- | Left 1-position cyclic shift of branches and elements.
    shiftCTL :: t -> t
    shiftCTL =  (</| 1)
    
    -- | Right 1-position cyclic shift of branches and elements.
    shiftCTR :: t -> t
    shiftCTR =  (|\> 1)

--------------------------------------------------------------------------------

{- |
  ShiftTree is class of trees, that can be shifted (may contain nodes with a
  different number of elements).
  
  Note that the class does not contain the function of the two-sided shift,
  and shift by a non-positive number of positions is tree itself.
-}
class (Tree t e) => ShiftTree t e | t -> e
  where
    {-# MINIMAL ((/<|)|shiftTL), ((|>\)|shiftTR) #-}
    
    -- | Left n-position non cyclic shift of branches and elements.
    (/<|) :: t -> Int -> t
    tree /<| n = n < 1 ? tree $ shiftTL tree /<| (n - 1)
    
    -- | Right n-position non cyclic shift of branches ans elements.
    (|>\) :: t -> Int -> t
    tree |>\ n = n < 1 ? tree $ shiftTR tree |>\ (n - 1)
    
    -- | Left 1-position non cyclic shift of branches and elements.
    shiftTL :: t -> t
    shiftTL =  (/<| 1)
    
    -- | Right 1-position non cyclic shift of branches and elements.
    shiftTR :: t -> t
    shiftTR =  (|>\ 1)

{- |
  AppendTree is a class of trees to which you can add elements and branches
  (may contain nodes with a different number of elements).
  
  Note that safety of operations is not guaranteed.
-}
class (Tree t e) => AppendTree t e | t -> e
  where
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

{- |
  MultiTree is a service class that represents trees of complex structure
  (B-tree, T-tree, R-tree, rose tree and their variations).
-}
class (ShiftTree t e, AppendTree t e) => MultiTree t e | t -> e

instance (ShiftTree t e, AppendTree t e) => MultiTree t e




