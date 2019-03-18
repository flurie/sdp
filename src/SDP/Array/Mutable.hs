{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

module SDP.Array.Mutable
(
  STArray (..), fill
)
where

import GHC.Base hiding (foldr, (++))
import SDP.SafePrelude
import Prelude ()
import GHC.Show
import GHC.ST

--------------------------------------------------------------------------------

data STArray s i e = STArray !i !i {-# UNPACK #-} !Int (MutableArray# s e)

type role STArray nominal nominal representational

instance Eq (STArray s i e)
  where
    (STArray _ _ _ a1#) == (STArray _ _ _ a2#) = isTrue# (sameMutableArray# a1# a2#)

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) next = \s1# -> case writeArray# marr# i# e s1# of s2# -> next s2#
