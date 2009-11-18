module Properties (tests) where

import Boilerplater
import Utilities

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Primitive
import qualified Data.Vector.Fusion.Stream as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Show.Functions ()
import Data.List           (foldl', foldl1', unfoldr, find, findIndex)

#define COMMON_CONTEXT(a, v) \
 VANILLA_CONTEXT(a, v), VECTOR_CONTEXT(a, v)

#define VANILLA_CONTEXT(a, v) \
  Eq a,     Show a,     Arbitrary a,     CoArbitrary a,     Modelled a,     Model a ~ a,       EqTestable a (Pty a), Pty a ~ Property

#define VECTOR_CONTEXT(a, v) \
  Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a), Modelled (v a), Model (v a) ~ [a], EqTestable (v a) (Pty (v a)), Pty (v a) ~ Property, V.Vector v a

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

testPolymorphicFunctions :: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT(Int, v)) => v a -> [Test]
testPolymorphicFunctions _ = $(testProperties [
        'prop_eq,

        'prop_length, 'prop_null,

        'prop_empty, 'prop_singleton, 'prop_replicate,
        'prop_cons, 'prop_snoc, 'prop_append, 'prop_copy,

        'prop_head, 'prop_last, 'prop_index,

        {- 'prop_slice, -} 'prop_init, 'prop_tail, 'prop_take, 'prop_drop,

        {- 'prop_accum, 'prop_write, 'prop_backpermute, -} 'prop_reverse,

        'prop_map, 'prop_zipWith, 'prop_zipWith3,
        'prop_filter, 'prop_takeWhile, 'prop_dropWhile,

        'prop_elem, 'prop_notElem,
        'prop_find, 'prop_findIndex,

        'prop_foldl, 'prop_foldl1, 'prop_foldl', 'prop_foldl1',
        'prop_foldr, 'prop_foldr1,

        'prop_prescanl, 'prop_prescanl',
        'prop_postscanl, 'prop_postscanl',
        'prop_scanl, 'prop_scanl', 'prop_scanl1, 'prop_scanl1',

        'prop_concatMap,
        'prop_unfoldr
    ])
  where
    -- Prelude
    prop_eq :: P (v a -> v a -> Bool) = (==) `eq` (==)

    prop_length :: P (v a -> Int)     = V.length `eq` length
    prop_null   :: P (v a -> Bool)    = V.null `eq` null

    prop_empty  :: P (v a)            = V.empty `eq` []
    prop_singleton :: P (a -> v a)    = V.singleton `eq` singleton
    prop_replicate :: P (Int -> a -> v a) = (\n _ -> n< 1000) ===> V.replicate `eq` replicate
    prop_cons      :: P (a -> v a -> v a) = V.cons `eq` (:)
    prop_snoc      :: P (v a -> a -> v a) = V.snoc `eq` snoc
    prop_append    :: P (v a -> v a -> v a) = (V.++) `eq` (++)
    prop_copy      :: P (v a -> v a)        = V.copy `eq` id

    prop_head         = not . V.null ===>
                        (V.head :: v a -> a)                          `eq` head
    prop_last         = not . V.null ===>
                        (V.last :: v a -> a)                          `eq` last
    prop_index        = forAll arbitrary $ \xs ->
                        not (V.null xs) ==>
                        forAll (choose (0, V.length xs-1)) $ \i ->
                        unP prop xs i
      where
        prop :: P (v a -> Int -> a) = (V.!) `eq` (!!)


    prop_slice        = forAll arbitrary                     $ \xs ->
                        forAll (choose (0, V.length xs))     $ \i ->
                        forAll (choose (0, V.length xs - i)) $ \n ->
                        unP prop xs i n
      where
        prop :: P (v a -> Int -> Int -> v a) = V.slice `eq` slice

    prop_tail         = not . V.null ===>
                        (V.tail :: v a -> v a)                        `eq` tail
    prop_init         = not . V.null ===>
                        (V.init :: v a -> v a)                        `eq` init
    prop_take         = (V.take :: Int -> v a -> v a)                 `eq` take
    prop_drop         = (V.drop :: Int -> v a -> v a)                 `eq` drop

    --prop_accum        = forAll arbitrary                         $ \f ->
    --                    forAll arbitrary                         $ \xs ->
    --                    forAll (index_value_pairs (V.length xs)) $ \ps ->
    --                    ((V.accum :: (a -> a -> a) -> v a -> [(Int,a)] -> v a)
    --                     `eq` accum) f xs ps
    --prop_write        = forAll arbitrary                         $ \xs ->
    --                    forAll (index_value_pairs (V.length xs)) $ \ps ->
    --                    (((V.//) :: v a -> [(Int,a)] -> v a) `eq` (//)) xs ps
    --prop_backpermute  = forAll arbitrary                         $ \xs ->
    --                    forAll (indices (V.length xs))           $ \is ->
    --                    ((V.backpermute :: v a -> v Int -> v a) `eq` backpermute)
    --                            xs (V.fromList is)
    prop_reverse      = (V.reverse :: v a -> v a)                     `eq` reverse

    prop_map          = (V.map :: (a -> a) -> v a -> v a)             `eq` map
    prop_zipWith      = (V.zipWith :: (a -> a -> a) -> v a -> v a -> v a) `eq` zipWith
    prop_zipWith3     = (V.zipWith3 :: (a -> a -> a -> a) -> v a -> v a -> v a -> v a) `eq` zipWith3

    prop_filter       = (V.filter :: (a -> Bool) -> v a -> v a)       `eq` filter
    prop_takeWhile    = (V.takeWhile :: (a -> Bool) -> v a -> v a)    `eq` takeWhile
    prop_dropWhile    = (V.dropWhile :: (a -> Bool) -> v a -> v a)    `eq` dropWhile

    prop_elem         = (V.elem :: a -> v a -> Bool)                  `eq` elem
    prop_notElem      = (V.notElem :: a -> v a -> Bool)               `eq` notElem
    prop_find         = (V.find :: (a -> Bool) -> v a -> Maybe a)        `eq` find
    prop_findIndex    = (V.findIndex :: (a -> Bool) -> v a -> Maybe Int) `eq` findIndex

    prop_foldl        = (V.foldl :: (a -> a -> a) -> a -> v a -> a)   `eq` foldl
    prop_foldl1       = notNull2 ===>
                        (V.foldl1 :: (a -> a -> a) -> v a -> a)       `eq` foldl1
    prop_foldl'       = (V.foldl' :: (a -> a -> a) -> a -> v a -> a)  `eq` foldl'
    prop_foldl1'      = notNull2 ===>
                        (V.foldl1' :: (a -> a -> a) -> v a -> a)      `eq` foldl1'
    prop_foldr        = (V.foldr :: (a -> a -> a) -> a -> v a -> a)   `eq` foldr
    prop_foldr1       = notNull2 ===>
                        (V.foldr1 :: (a -> a -> a) -> v a -> a)       `eq` foldr1

    prop_prescanl     = (V.prescanl :: (a -> a -> a) -> a -> v a -> v a) `eq` prescanl
    prop_prescanl'    = (V.prescanl' :: (a -> a -> a) -> a -> v a -> v a) `eq` prescanl
    prop_postscanl    = (V.postscanl :: (a -> a -> a) -> a -> v a -> v a) `eq` postscanl
    prop_postscanl'   = (V.postscanl' :: (a -> a -> a) -> a -> v a -> v a) `eq` postscanl
    prop_scanl        = (V.scanl :: (a -> a -> a) -> a -> v a -> v a) `eq` scanl
    prop_scanl'       = (V.scanl' :: (a -> a -> a) -> a -> v a -> v a) `eq` scanl
    prop_scanl1       = notNull2 ===>
                        (V.scanl1 :: (a -> a -> a) -> v a -> v a)     `eq` scanl1
    prop_scanl1'      = notNull2 ===>
                        (V.scanl1' :: (a -> a -> a) -> v a -> v a)    `eq` scanl1
 
    prop_concatMap    = forAll arbitrary $ \xs ->
                        forAll (sized (\n -> resize (n `div` V.length xs) arbitrary)) $ \f -> unP prop f xs
      where
        prop :: P ((a -> v a) -> v a -> v a) = V.concatMap `eq` concatMap

    --prop_span         = (V.span :: (a -> Bool) -> v a -> (v a, v a))  `eq2` span
    --prop_break        = (V.break :: (a -> Bool) -> v a -> (v a, v a)) `eq2` break
    --prop_splitAt      = (V.splitAt :: Int -> v a -> (v a, v a))       `eq2` splitAt
    --prop_all          = (V.all :: (a -> Bool) -> v a -> Bool)         `eq2` all
    --prop_any          = (V.any :: (a -> Bool) -> v a -> Bool)         `eq2` any

    -- Data.List
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
                        `eq` (\n f a -> unfoldr (limitUnfolds f) (a, n))


testTuplyFunctions:: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT((a, a), v), VECTOR_CONTEXT((a, a, a), v)) => v a -> [Test]
testTuplyFunctions _ = $(testProperties ['prop_zip, 'prop_zip3, 'prop_unzip, 'prop_unzip3])
  where
    prop_zip          = (V.zip :: v a -> v a -> v (a, a))             `eq` zip
    prop_zip3         = (V.zip3 :: v a -> v a -> v a -> v (a, a, a))  `eq` zip3
    prop_unzip        = (V.unzip :: v (a, a) -> (v a, v a))           `eq` unzip
    prop_unzip3       = (V.unzip3 :: v (a, a, a) -> (v a, v a, v a))  `eq` unzip3

testOrdFunctions :: forall a v. (COMMON_CONTEXT(a, v), Ord a, Ord (v a)) => v a -> [Test]
testOrdFunctions _ = $(testProperties ['prop_compare, 'prop_maximum, 'prop_minimum])
  where
    prop_compare      = (compare :: v a -> v a -> Ordering) `eq` compare
    prop_maximum      = not . V.null ===>
                        (V.maximum :: v a -> a)             `eq` maximum
    prop_minimum      = not . V.null ===>
                        (V.minimum :: v a -> a)             `eq` minimum

testEnumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Enum a, Ord a, Num a) => v a -> [Test]
testEnumFunctions _ = $(testProperties ['prop_enumFromTo {- 'prop_enumFromThenTo -}])
  where
    prop_enumFromTo = forAll arbitrary $ \m ->
                      forAll (elements [-2 .. 100]) $ \n ->
                      unP prop m (m+n)
      where
        prop  :: P (a -> a -> v a) = V.enumFromTo `eq` enumFromTo
    -- prop_enumFromThenTo = \i j n -> fromEnum i < fromEnum j ==> ((V.enumFromThenTo :: a -> a -> a -> v a) `eq` enumFromThenTo) i j n

testBoolFunctions :: forall v. (COMMON_CONTEXT(Bool, v)) => v Bool -> [Test]
testBoolFunctions _ = $(testProperties ['prop_and, 'prop_or])
  where
    prop_and          = (V.and :: v Bool -> Bool) `eq` and
    prop_or           = (V.or :: v Bool -> Bool)  `eq` or

testNumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Num a) => v a -> [Test]
testNumFunctions _ = $(testProperties ['prop_sum, 'prop_product])
  where
    prop_sum          = (V.sum :: v a -> a)     `eq` sum
    prop_product      = (V.product :: v a -> a) `eq` product

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
        testTuplyFunctions,
        testNestedVectorFunctions
    ]

testBoolBoxedVector dummy = testGeneralBoxedVector dummy ++ testBoolFunctions dummy
testNumericBoxedVector dummy = testGeneralBoxedVector dummy ++ testNumFunctions dummy ++ testEnumFunctions dummy

testGeneralPrimitiveVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions
    ]

testBoolPrimitiveVector dummy = testGeneralPrimitiveVector dummy ++ testBoolFunctions dummy
testNumericPrimitiveVector dummy = testGeneralPrimitiveVector dummy ++ testNumFunctions dummy ++ testEnumFunctions dummy

tests = [
        testGroup "Data.Vector.Vector (Bool)"           (testBoolBoxedVector      (undefined :: Data.Vector.Vector Bool)),
        testGroup "Data.Vector.Vector (Int)"            (testNumericBoxedVector   (undefined :: Data.Vector.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Int)"    (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Float)"  (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Float)),
        testGroup "Data.Vector.Primitive.Vector (Double)" (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Double))
    ]
