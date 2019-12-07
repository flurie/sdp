{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}

{- |
    Module      :  SDP.Tree
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Tree@ provides classes for tree operations.
-}
module SDP.Tree
(
  -- * Common tree operations
  Tree (..),
  
  -- * Advanced tree operations
  ShiftTree (..), AppendTree (..), MultiTree,
  
  -- * Patterns
  pattern (:/*\:),
  pattern (:/+),
  pattern (:+\),
  pattern Leaf,
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
    
    -- | List of node elements.
    nodeElems :: t -> [e]
    nodeElems node = nodeElem node <$> [0 .. elemCount node - 1]
    
    -- | List of node branches.
    branches :: t -> [t]
    branches node = branch node <$> [0 .. elemCount node - 1]
    
    -- | Tree normalization function, may fail.
    fixTree :: t -> t
    
    -- | Returns n-th element of node (from 0), may fail.
    nodeElem :: t -> Int -> e
    nodeElem node = (nodeElems node !)
    
    -- | Applies function to n-th element.
    onElem :: (e -> e) -> Int -> t -> t
    onElem f n tree@(~(es :/*\: bs)) = indexIn es n ? go n es :/*\: bs $ tree
      where
        go _ [] = []
        go i (x : xs) = i == 0 ? f x : xs $ x : go (i - 1) xs
    
    -- | Returns count of node elements, nonnegative.
    elemCount :: t -> Int
    elemCount =  length . nodeElems
    
    -- | Returns n-th branch of node (from 0), may fail.
    branch :: t -> Int -> t
    branch node = (branches node !)
    
    -- | Applies function to n-th branch.
    onBranch :: (t -> t) -> Int -> t -> t
    onBranch f n tree@(~(es :/*\: bs)) = indexIn bs n ? es :/*\: go n bs $ tree
      where
        go _    []    = []
        go i (x : xs) = i == 0 ? f x : xs $ x : go (i - 1) xs
    
    -- | Returns count of branches, nonnegative.
    branchCount :: t -> Int
    branchCount = length . branches
    
    {- Common operations. -}
    
    isLeaf :: t -> Bool
    isLeaf node = branchCount node == 0
    
    -- | Number of element in current node from 0.
    elemPos :: e -> t -> Maybe Int
    elemPos e node = (== e) .$ nodeElems node
    
    -- | Returns number of branch or element that (may) contain given value.
    branchPos :: e -> t -> Int
    branchPos e' = go 0 e' . nodeElems
      where
        go n _    []    = n
        go n e (x : xs) = e <= x ? n $ go (n + 1) e xs
    
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
    (|\>) :: Int -> t -> t
    n |\> tree = n < 1 ? tree $ (n - 1) |\> shiftCTR tree
    
    -- | Left 1-position cyclic shift of branches and elements.
    shiftCTL :: t -> t
    shiftCTL =  (</| 1)
    
    -- | Right 1-position cyclic shift of branches and elements.
    shiftCTR :: t -> t
    shiftCTR =  (1 |\>)

--------------------------------------------------------------------------------

-- | Unidirectional pattern synonym for isLeaf.
pattern Leaf :: (Tree t e) => t
pattern Leaf <- (isLeaf -> True)

-- | (:/*\:) is generic pattern synonym for 'toTree' and 'viewTree'.
pattern (:/*\:) :: (Tree t e) => [e] -> [t] -> t
pattern es :/*\: bs <- (viewTree -> (es, bs)) where es :/*\: bs = toTree es bs

viewTree :: (Tree t e) => t -> ([e], [t])
viewTree node = (nodeElems node, branches node)

-- | (:/+) is generic pattern synonym for ('/+') and left-side view.
pattern (:/+) :: (ShiftTree t e, AppendTree t e) => (t, e) -> t -> t
pattern ebr :/+ node <- (viewBranchL -> (ebr, node)) where ebr :/+ node = ebr /+ node

viewBranchL :: (ShiftTree t e, AppendTree t e) => t -> ((t, e), t)
viewBranchL node = ((branch node 0, nodeElem node 0), shiftTL node)

-- | (:+\) is generic pattern synonym for ('/+') and right-side view.
pattern (:+\) :: (ShiftTree t e, AppendTree t e) => t -> (e, t) -> t
pattern node :+\ bre <- (viewBranchR -> (node, bre)) where node :+\ bre = node +\ bre

viewBranchR :: (ShiftTree t e, AppendTree t e) => t -> (t, (e, t))
viewBranchR node = (shiftTL node, (nodeElem node (elemCount node - 1), branch node (branchCount node - 1)))

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
    (|>\) :: Int -> t -> t
    n |>\ tree = n < 1 ? tree $ (n - 1) |>\ shiftTR tree
    
    -- | Left 1-position non cyclic shift of branches and elements.
    shiftTL :: t -> t
    shiftTL =  (/<| 1)
    
    -- | Right 1-position non cyclic shift of branches and elements.
    shiftTR :: t -> t
    shiftTR =  (1 |>\)

{- |
  AppendTree is a class of trees to which you can add elements and branches
  (may contain nodes with a different number of elements).
  
  Note that safety of operations is not guaranteed.
-}
class (Tree t e) => AppendTree t e | t -> e
  where
    -- | @(/+)@ appends pair of branch and element to left side of the tree.
    (/+) :: (t, e) -> t -> t
    (br, e) /+ node = toTree (e :> nodeElems node) (br :> branches node)
    
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
class    (ShiftTree t e, AppendTree t e) => MultiTree t e | t -> e
instance (ShiftTree t e, AppendTree t e) => MultiTree t e

