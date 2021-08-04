{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Vector.Internal.Check
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Bounds checking infrastructure
--
module Data.Vector.Internal.Check (
  HasCallStack,
  Checks(..), doChecks,

  internalError,
  check, checkIndex, checkLength, checkSlice
) where

import GHC.Base( Int(..) )
import GHC.Prim( Int# )
import Prelude hiding( error, (&&), (||), not )
import qualified Prelude as P

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack)
#else
import Data.CallStack (HasCallStack)
#endif

-- NOTE: This is a workaround for GHC's weird behaviour where it doesn't inline
-- these functions into unfoldings which makes the intermediate code size
-- explode. See http://hackage.haskell.org/trac/ghc/ticket/5539.
infixr 2 ||
infixr 3 &&

not :: Bool -> Bool
{-# INLINE not #-}
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
{-# INLINE (&&) #-}
False && _ = False
True && x = x

(||) :: Bool -> Bool -> Bool
{-# INLINE (||) #-}
True || _ = True
False || x = x


data Checks = Bounds | Unsafe | Internal deriving( Eq )

doBoundsChecks :: Bool
#ifdef VECTOR_BOUNDS_CHECKS
doBoundsChecks = True
#else
doBoundsChecks = False
#endif

doUnsafeChecks :: Bool
#ifdef VECTOR_UNSAFE_CHECKS
doUnsafeChecks = True
#else
doUnsafeChecks = False
#endif

doInternalChecks :: Bool
#ifdef VECTOR_INTERNAL_CHECKS
doInternalChecks = True
#else
doInternalChecks = False
#endif


doChecks :: Checks -> Bool
{-# INLINE doChecks #-}
doChecks Bounds   = doBoundsChecks
doChecks Unsafe   = doUnsafeChecks
doChecks Internal = doInternalChecks

internalError :: HasCallStack => String -> a
{-# NOINLINE internalError #-}
internalError msg
  = P.error $ unlines
        ["*** Internal error in package vector ***"
        ,"*** Please submit a bug report at http://github.com/haskell/vector"
        ,msg]


checkError :: HasCallStack => Checks -> String -> a
{-# NOINLINE checkError #-}
checkError kind msg
  = case kind of
      Internal -> internalError msg
      _ -> P.error msg

check :: HasCallStack => Checks -> String -> Bool -> a -> a
{-# INLINE check #-}
check kind msg cond x
  | not (doChecks kind) || cond = x
  | otherwise = checkError kind msg

checkIndex_msg :: Int -> Int -> String
{-# INLINE checkIndex_msg #-}
checkIndex_msg (I# i#) (I# n#) = checkIndex_msg# i# n#

checkIndex_msg# :: Int# -> Int# -> String
{-# NOINLINE checkIndex_msg# #-}
checkIndex_msg# i# n# = "index out of bounds " ++ show (I# i#, I# n#)

checkIndex :: Checks -> Int -> Int -> a -> a
{-# INLINE checkIndex #-}
checkIndex kind i n x
  = check kind (checkIndex_msg i n) (i >= 0 && i<n) x


checkLength_msg :: Int -> String
{-# INLINE checkLength_msg #-}
checkLength_msg (I# n#) = checkLength_msg# n#

checkLength_msg# :: Int# -> String
{-# NOINLINE checkLength_msg# #-}
checkLength_msg# n# = "negative length " ++ show (I# n#)

checkLength :: Checks -> Int -> a -> a
{-# INLINE checkLength #-}
checkLength kind n = check kind (checkLength_msg n) (n >= 0)


checkSlice_msg :: Int -> Int -> Int -> String
{-# INLINE checkSlice_msg #-}
checkSlice_msg (I# i#) (I# m#) (I# n#) = checkSlice_msg# i# m# n#

checkSlice_msg# :: Int# -> Int# -> Int# -> String
{-# NOINLINE checkSlice_msg# #-}
checkSlice_msg# i# m# n# = "invalid slice " ++ show (I# i#, I# m#, I# n#)

checkSlice :: Checks -> Int -> Int -> Int -> a -> a
{-# INLINE checkSlice #-}
checkSlice kind i m n x
  = check kind (checkSlice_msg i m n) (i >= 0 && m >= 0 && m <= n - i) x

