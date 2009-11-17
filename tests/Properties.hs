module Properties (tests) where

import Boilerplater
import Utilities

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Primitive
import qualified Data.Vector.Fusion.Stream as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck

import Text.Show.Functions ()
import Data.List           (foldl', foldl1', unfoldr, find, findIndex)

#define COMMON_CONTEXT(a, v) \
 VANILLA_CONTEXT(a, v), VECTOR_CONTEXT(a, v)

#define VANILLA_CONTEXT(a, v) \
  Eq a,     Show a,     Arbitrary a,     Model a a

#define VECTOR_CONTEXT(a, v) \
  Eq (v a), Show (v a), Arbitrary (v a), Model (v a) [a], V.Vector v a


-- TODO: implement Vector equivalents of list functions for some of the commented out properties

-- TODO: test and implement some of these other Prelude functions:
--  mapM *
--  mapM_ *
--  sequence
--  sequence_
--  sum *
--  product *
--  scanl *
--  scanl1 *
--  scanr *
--  scanr1 *
--  lookup *
--  lines
--  words
--  unlines
--  unwords
-- NB: this is an exhaustive list of all Prelude list functions that make sense for vectors.
-- Ones with *s are the most plausible candidates.

-- TODO: add tests for the other extra functions
-- IVector exports still needing tests:
--  copy,
--  slice,
--  (//), update, bpermute,
--  prescanl, prescanl',
--  new,
--  unsafeSlice, unsafeIndex,
--  vlength, vnew

-- TODO: test non-IVector stuff?

testSanity :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testSanity _ = [
        testProperty "fromList.toList == id" prop_fromList_toList,
        testProperty "toList.fromList == id" prop_toList_fromList,
        testProperty "unstream.stream == id" prop_unstream_stream,
        testProperty "stream.unstream == id" prop_stream_unstream
    ]
  where
    prop_fromList_toList (v :: v a)        = (V.fromList . V.toList)                        v == v
    prop_toList_fromList (l :: [a])        = ((V.toList :: v a -> [a]) . V.fromList)        l == l
    prop_unstream_stream (v :: v a)        = (V.unstream . V.stream)                        v == v
    prop_stream_unstream (s :: S.Stream a) = ((V.stream :: v a -> S.Stream a) . V.unstream) s == s

testPolymorphicFunctions :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testPolymorphicFunctions _ = $(testProperties [
        'prop_eq, 'prop_length, 'prop_null, 'prop_reverse,
        'prop_append, 'prop_concatMap,
        'prop_empty, 'prop_cons,
        'prop_head, 'prop_tail, 'prop_init, 'prop_last,
        'prop_drop, 'prop_dropWhile, 'prop_take, 'prop_takeWhile,
        'prop_filter, 'prop_map, 'prop_replicate,
        'prop_zipWith, 'prop_zipWith3,
        'prop_elem, 'prop_notElem,
        'prop_foldr, 'prop_foldl, 'prop_foldr1, 'prop_foldl1,
        'prop_foldl', 'prop_foldl1',
        'prop_find, 'prop_findIndex,
        'prop_unfoldr,
        'prop_singleton, 'prop_snoc
    ])
  where
    -- Prelude
    prop_eq           = ((==) :: v a -> v a -> Bool)                  `eq2` (==)
    prop_length       = (V.length :: v a -> Int)                      `eq1` length
    prop_null         = (V.null :: v a -> Bool)                       `eq1` null
    prop_reverse      = (V.reverse :: v a -> v a)                     `eq1` reverse
    prop_append       = ((V.++) :: v a -> v a -> v a)                 `eq2` (++)
    prop_concatMap    = (V.concatMap :: (a -> v a) -> v a -> v a)     `eq2` concatMap
    prop_empty        = (V.empty :: v a)                              `eq0` []
    prop_cons         = (V.cons :: a -> v a -> v a)                   `eq2` (:)
    --prop_index        = compare (V.!) to (!!)
    prop_head         = (V.head :: v a -> a)                          `eqNotNull1` head
    prop_tail         = (V.tail :: v a -> v a)                        `eqNotNull1` tail
    prop_init         = (V.init :: v a -> v a)                        `eqNotNull1` init
    prop_last         = (V.last :: v a -> a)                          `eqNotNull1` last
    prop_drop         = (V.drop :: Int -> v a -> v a)                 `eq2` drop
    prop_dropWhile    = (V.dropWhile :: (a -> Bool) -> v a -> v a)    `eq2` dropWhile
    prop_take         = (V.take :: Int -> v a -> v a)                 `eq2` take
    prop_takeWhile    = (V.takeWhile :: (a -> Bool) -> v a -> v a)    `eq2` takeWhile
    prop_filter       = (V.filter :: (a -> Bool) -> v a -> v a)       `eq2` filter
    prop_map          = (V.map :: (a -> a) -> v a -> v a)             `eq2` map
    prop_replicate    = (V.replicate :: Int -> a -> v a)              `eq2` replicate
    prop_zipWith      = (V.zipWith :: (a -> a -> a) -> v a -> v a -> v a) `eq3` zipWith
    prop_zipWith3     = (V.zipWith3 :: (a -> a -> a -> a) -> v a -> v a -> v a -> v a) `eq4` zipWith3
    --prop_span         = (V.span :: (a -> Bool) -> v a -> (v a, v a))  `eq2` span
    --prop_break        = (V.break :: (a -> Bool) -> v a -> (v a, v a)) `eq2` break
    --prop_splitAt      = (V.splitAt :: Int -> v a -> (v a, v a))       `eq2` splitAt
    prop_elem         = (V.elem :: a -> v a -> Bool)                  `eq2` elem
    prop_notElem      = (V.notElem :: a -> v a -> Bool)               `eq2` notElem
    prop_foldr        = (V.foldr :: (a -> a -> a) -> a -> v a -> a)   `eq3` foldr
    prop_foldl        = (V.foldl :: (a -> a -> a) -> a -> v a -> a)   `eq3` foldl
    prop_foldr1       = (V.foldr1 :: (a -> a -> a) -> v a -> a)       `eqNotNull2` foldr1
    prop_foldl1       = (V.foldl1 :: (a -> a -> a) -> v a -> a)       `eqNotNull2` foldl1
    --prop_all          = (V.all :: (a -> Bool) -> v a -> Bool)         `eq2` all
    --prop_any          = (V.any :: (a -> Bool) -> v a -> Bool)         `eq2` any

    -- Data.List
    prop_foldl'       = (V.foldl' :: (a -> a -> a) -> a -> v a -> a)     `eq3` foldl'
    prop_foldl1'      = (V.foldl1' :: (a -> a -> a) -> v a -> a)         `eqNotNull2` foldl1'
    prop_find         = (V.find :: (a -> Bool) -> v a -> Maybe a)        `eq2` find
    prop_findIndex    = (V.findIndex :: (a -> Bool) -> v a -> Maybe Int) `eq2` findIndex
    --prop_findIndices  = V.findIndices `eq2` (findIndices :: (a -> Bool) -> v a -> v Int)
    --prop_isPrefixOf   = V.isPrefixOf  `eq2` (isPrefixOf  :: v a -> v a -> Bool)
    --prop_elemIndex    = V.elemIndex   `eq2` (elemIndex   :: a -> v a -> Maybe Int)
    --prop_elemIndices  = V.elemIndices `eq2` (elemIndices :: a -> v a -> v Int)
    --
    --prop_mapAccumL  = eq3
    --    (V.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
    -- 
    --prop_mapAccumR  = eq3
    --    (V.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

    -- Because the vectors are strict, we need to be totally sure that the unfold eventually terminates. This
    -- is achieved by injecting our own bit of state into the unfold - the maximum number of unfolds allowed.
    limitUnfolds f (theirs, ours) | ours >= 0
                                  , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
                                  | otherwise                       = Nothing
    prop_unfoldr      = ((\n f a -> V.unfoldr (limitUnfolds f) (a, n)) :: Int -> ((Int, Int) -> Maybe (a, (Int, Int))) -> (Int, Int) -> v a)
                        `eq3` (\n f a -> unfoldr (limitUnfolds f) (a, n))

    -- Extras
    singleton x = [x]
    prop_singleton = (V.singleton :: a -> v a) `eq1` singleton
    
    snoc xs x = xs ++ [x]
    prop_snoc = (V.snoc :: v a -> a -> v a) `eq2` snoc

testTuplyFunctions:: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT((a, a), v), VECTOR_CONTEXT((a, a, a), v)) => v a -> [Test]
testTuplyFunctions _ = $(testProperties ['prop_zip, 'prop_zip3, 'prop_unzip, 'prop_unzip3])
  where
    prop_zip          = (V.zip :: v a -> v a -> v (a, a))             `eq2` zip
    prop_zip3         = (V.zip3 :: v a -> v a -> v a -> v (a, a, a))  `eq3` zip3
    prop_unzip        = (V.unzip :: v (a, a) -> (v a, v a))           `eq1` unzip
    prop_unzip3       = (V.unzip3 :: v (a, a, a) -> (v a, v a, v a))  `eq1` unzip3

testOrdFunctions :: forall a v. (COMMON_CONTEXT(a, v), Ord a, Ord (v a)) => v a -> [Test]
testOrdFunctions _ = $(testProperties ['prop_compare, 'prop_maximum, 'prop_minimum])
  where
    prop_compare      = (compare :: v a -> v a -> Ordering) `eq2` compare
    prop_maximum      = (V.maximum :: v a -> a)             `eqNotNull1` maximum
    prop_minimum      = (V.minimum :: v a -> a)             `eqNotNull1` minimum

testEnumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Enum a) => v a -> [Test]
testEnumFunctions _ = $(testProperties ['prop_enumFromTo, 'prop_enumFromThenTo])
  where
    prop_enumFromTo     =                                        (V.enumFromTo :: a -> a -> v a)          `eq2` enumFromTo
    prop_enumFromThenTo = \i j n -> fromEnum i < fromEnum j ==> ((V.enumFromThenTo :: a -> a -> a -> v a) `eq3` enumFromThenTo) i j n

testBoolFunctions :: forall v. (COMMON_CONTEXT(Bool, v)) => v Bool -> [Test]
testBoolFunctions _ = $(testProperties ['prop_and, 'prop_or])
  where
    prop_and          = (V.and :: v Bool -> Bool) `eq1` and
    prop_or           = (V.or :: v Bool -> Bool)  `eq1` or

testNumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Num a) => v a -> [Test]
testNumFunctions _ = $(testProperties ['prop_sum, 'prop_product])
  where
    prop_sum          = (V.sum :: v a -> a)     `eq1` sum
    prop_product      = (V.product :: v a -> a) `eq1` product

testNestedVectorFunctions :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testNestedVectorFunctions _ = $(testProperties [])
  where
    -- Prelude
    --prop_concat       = (V.concat :: [v a] -> v a)                    `eq1` concat
    
    -- Data.List
    --prop_transpose    = V.transpose   `eq1` (transpose   :: [v a] -> [v a])
    --prop_group        = V.group       `eq1` (group       :: v a -> [v a])
    --prop_inits        = V.inits       `eq1` (inits       :: v a -> [v a])
    --prop_tails        = V.tails       `eq1` (tails       :: v a -> [v a])


testGeneralBoxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testEnumFunctions,
        testTuplyFunctions,
        testNestedVectorFunctions
    ]

testBoolBoxedVector dummy = testGeneralBoxedVector dummy ++ testBoolFunctions dummy
testNumericBoxedVector dummy = testGeneralBoxedVector dummy ++ testNumFunctions dummy

testGeneralPrimitiveVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testEnumFunctions
    ]

testBoolPrimitiveVector dummy = testGeneralPrimitiveVector dummy ++ testBoolFunctions dummy
testNumericPrimitiveVector dummy = testGeneralPrimitiveVector dummy ++ testNumFunctions dummy

tests = [
        testGroup "Data.Vector.Vector (Bool)"           (testBoolBoxedVector      (undefined :: Data.Vector.Vector Bool)),
        testGroup "Data.Vector.Vector (Int)"            (testNumericBoxedVector   (undefined :: Data.Vector.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Int)"    (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Float)"  (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Float)),
        testGroup "Data.Vector.Primitive.Vector (Double)" (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Double))
    ]
