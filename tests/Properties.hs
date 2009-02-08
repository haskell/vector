{-# LANGUAGE CPP, FlexibleContexts, Rank2Types, ScopedTypeVariables, PatternGuards #-}

module Properties (tests) where

import Utilities

import qualified Data.Vector.IVector as V
import qualified Data.Vector
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Fusion.Stream as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck

import Text.Show.Functions
import Data.List (foldl', foldl1', unfoldr, find, findIndex)

#define HUGE_CLASS_CONTEXT(a, v) \
  Enum a, \
  Eq a,     Ord a, \
  Eq (v a), Ord (v a), \
  Show a,        Arbitrary a,        Model a a, \
  Show (v a),    Arbitrary (v a),    Model (v a) [a],       V.IVector v a, \
  Show (v Bool), Arbitrary (v Bool), Model (v Bool) [Bool], V.IVector v Bool


testVectorType :: forall a v. (HUGE_CLASS_CONTEXT(a, v)) => v a -> [Test]
testVectorType dummy = [
        testGroup "Sanity checks"                (testSanity dummy),
        testGroup "Semantics should match lists" (testVersusLists dummy),
        testGroup "Non-list functions correct"   (testExtraFunctions dummy)
    ]

testSanity :: forall a v. (HUGE_CLASS_CONTEXT(a, v)) => v a -> [Test]
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

testVersusLists :: forall a v. (HUGE_CLASS_CONTEXT(a, v)) => v a -> [Test]
testVersusLists _ = [
        testGroup "Prelude"   prelude_tests,
        testGroup "Data.List" data_list_tests
    ]
  where
    prelude_tests = [
            --testProperty "concat"       prop_concat,
            testProperty "length"       prop_length,
            testProperty "null"         prop_null,
            --testProperty "reverse"      prop_reverse,
            --testProperty "all"          prop_all,
            --testProperty "any"          prop_any,
            testProperty "and"          prop_and,
            testProperty "or"           prop_or,
            testProperty "(++)"         prop_append,
            --testProperty "break"        prop_break,
            testProperty "concatMap"    prop_concatMap,
            testProperty "[]"           prop_empty,
            testProperty "(:)"          prop_cons,
            testProperty "drop"         prop_drop,
            testProperty "dropWhile"    prop_dropWhile,
            testProperty "take"         prop_take,
            testProperty "takeWhile"    prop_takeWhile,
            testProperty "filter"       prop_filter,
            testProperty "map"          prop_map,
            testProperty "zipWith"      prop_zipWith,
            testProperty "replicate"    prop_replicate,
            --testProperty "span"         prop_span,
            --testProperty "splitAt"      prop_splitAt,
            testProperty "elem"         prop_elem,
            testProperty "notElem"      prop_notElem,
            testProperty "foldr"        prop_foldr,
            testProperty "foldl"        prop_foldl,
            testProperty "foldl'"       prop_foldl',
            --testProperty "lines"        prop_lines,
            testProperty "foldr1"       prop_foldr1,
            testProperty "foldl1"       prop_foldl1,
            testProperty "foldl1'"      prop_foldl1',
            testProperty "head"         prop_head,
            testProperty "tail"         prop_tail,
            testProperty "init"         prop_init,
            testProperty "last"         prop_last,
            --testProperty "maximum"      prop_maximum,
            --testProperty "minimum"      prop_minimum,
            testProperty "(==)"         prop_eq,
            testProperty "compare"      prop_compare
        ]
    
    -- TODO: implement Vector equivalents for the commented out list functions from Prelude
    --prop_concat       = (V.concat :: [v a] -> v a)                    `eq1` concat
    prop_length       = (V.length :: v a -> Int)                      `eq1` length
    prop_null         = (V.null :: v a -> Bool)                       `eq1` null
    --prop_reverse      = (V.reverse :: v a -> v a)                     `eq1` reverse
    --prop_all          = (V.all :: (a -> Bool) -> v a -> Bool)         `eq2` all
    --prop_any          = (V.any :: (a -> Bool) -> v a -> Bool)         `eq2` any
    prop_and          = (V.and :: v Bool -> Bool)                     `eq1` and
    prop_or           = (V.or :: v Bool -> Bool)                      `eq1` or
    prop_append       = ((V.++) :: v a -> v a -> v a)                 `eq2` (++)
    --prop_break        = (V.break :: (a -> Bool) -> v a -> (v a, v a)) `eq2` break
    prop_concatMap    = (V.concatMap :: (a -> v a) -> v a -> v a)     `eq2` concatMap
    prop_empty        = (V.empty :: v a)                              `eq0` []
    prop_cons         = (V.cons :: a -> v a -> v a)                   `eq2` (:)
    prop_drop         = (V.drop :: Int -> v a -> v a)                 `eq2` drop
    prop_dropWhile    = (V.dropWhile :: (a -> Bool) -> v a -> v a)    `eq2` dropWhile
    prop_take         = (V.take :: Int -> v a -> v a)                 `eq2` take
    prop_takeWhile    = (V.takeWhile :: (a -> Bool) -> v a -> v a)    `eq2` takeWhile
    prop_filter       = (V.filter :: (a -> Bool) -> v a -> v a)       `eq2` filter
    prop_map          = (V.map :: (a -> a) -> v a -> v a)             `eq2` map
    prop_zipWith      = (V.zipWith :: (a -> a -> a) -> v a -> v a -> v a) `eq3` zipWith
    prop_replicate    = (V.replicate :: Int -> a -> v a)              `eq2` replicate
    --prop_span         = (V.span :: (a -> Bool) -> v a -> (v a, v a))  `eq2` span
    --prop_splitAt      = (V.splitAt :: Int -> v a -> (v a, v a))       `eq2` splitAt
    prop_elem         = (V.elem :: a -> v a -> Bool)                  `eq2` elem
    prop_notElem      = (V.notElem :: a -> v a -> Bool)               `eq2` notElem
    --prop_lines        = (V.lines :: String -> [String])               `eq1` lines
    prop_foldr        = (V.foldr :: (a -> a -> a) -> a -> v a -> a)   `eq3` foldr
    prop_foldl        = (V.foldl :: (a -> a -> a) -> a -> v a -> a)   `eq3` foldl
    prop_foldr1       = (V.foldr1 :: (a -> a -> a) -> v a -> a)       `eqNotNull2` foldr1
    prop_foldl1       = (V.foldl1 :: (a -> a -> a) -> v a -> a)       `eqNotNull2` foldl1
    prop_head         = (V.head :: v a -> a)                          `eqNotNull1` head
    prop_tail         = (V.tail :: v a -> v a)                        `eqNotNull1` tail
    prop_init         = (V.init :: v a -> v a)                        `eqNotNull1` init
    prop_last         = (V.last :: v a -> a)                          `eqNotNull1` last
    --prop_maximum      = (V.maximum :: v a -> a)                       `eqNotNull1` maximum
    --prop_minimum      = (V.minimum :: v a -> a)                       `eqNotNull1` minimum
    prop_eq           = ((==) :: v a -> v a -> Bool)                  `eq2` (==)
    prop_compare      = (compare :: v a -> v a -> Ordering)           `eq2` compare
    prop_enumFromTo   = (V.enumFromTo :: a -> a -> v a)               `eq2` enumFromTo
    prop_enumFromThenTo = (V.enumFromThenTo :: a -> a -> a -> v a)    `eq3` enumFromThenTo
    
    data_list_tests = [
            testProperty "foldl'"       prop_foldl',
            testProperty "foldl1'"      prop_foldl1',
            testProperty "unfoldr"      prop_unfoldr,
            testProperty "find"         prop_find,
            testProperty "findIndex"    prop_findIndex
            --testProperty "transpose"    prop_transpose,
            --testProperty "group"        prop_group,
            --testProperty "inits"        prop_inits,
            --testProperty "tails"        prop_tails,
            --testProperty "findIndices"  prop_findIndices,
            --testProperty "isPrefixOf"   prop_isPrefixOf,
            --testProperty "elemIndex"    prop_elemIndex,
            --testProperty "elemIndices"  prop_elemIndices,
            --testProperty "mapAccumL"    prop_mapAccumL,
            --testProperty "mapAccumR"    prop_mapAccumR,
        ]
    
    -- TODO: implement Vector equivalents for some of the commented out list functions from Data.List
    prop_foldl'       = (V.foldl' :: (a -> a -> a) -> a -> v a -> a)     `eq3` foldl'
    prop_foldl1'      = (V.foldl1' :: (a -> a -> a) -> v a -> a)         `eqNotNull2` foldl1'
    prop_find         = (V.find :: (a -> Bool) -> v a -> Maybe a)        `eq2` find
    prop_findIndex    = (V.findIndex :: (a -> Bool) -> v a -> Maybe Int) `eq2` findIndex
    --prop_transpose    = V.transpose   `eq1` (transpose   :: [v a] -> [v a])
    --prop_group        = V.group       `eq1` (group       :: v a -> [v a])
    --prop_inits        = V.inits       `eq1` (inits       :: v a -> [v a])
    --prop_tails        = V.tails       `eq1` (tails       :: v a -> [v a])
    --prop_findIndices  = V.findIndices `eq2` (findIndices :: (a -> Bool) -> v a -> [Int])
    --prop_isPrefixOf   = V.isPrefixOf  `eq2` (isPrefixOf  :: v a -> v a -> Bool)
    --prop_elemIndex    = V.elemIndex   `eq2` (elemIndex   :: a -> v a -> Maybe Int)
    --prop_elemIndices  = V.elemIndices `eq2` (elemIndices :: a -> v a -> [Int])
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

testExtraFunctions :: forall a v. (HUGE_CLASS_CONTEXT(a, v)) => v a -> [Test]
testExtraFunctions _ =  [
        testProperty "singleton"    prop_singleton,
        testProperty "snoc"         prop_snoc
    ]
  where
    singleton x = [x]
    prop_singleton = (V.singleton :: a -> v a) `eq1` singleton
    
    snoc xs x = xs ++ [x]
    prop_snoc = (V.snoc :: v a -> a -> v a) `eq2` snoc
    
    -- TODO: add tests for the other extra functions
    -- IVector exports still needing tests:
    --  copy,
    --  (!),
    --  slice,
    --  (//), update, bpermute,
    --  zip,
    --  prescanl, prescanl',
    --  new,
    --  unsafeSlice, unsafeIndex,
    --  vlength, vnew

-- TODO: test non-IVector stuff?
tests = [
        testGroup "Data.Vector.Vector"                (testVectorType (undefined :: Data.Vector.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Int)"  (testVectorType (undefined :: Data.Vector.Unboxed.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Bool)" (testVectorType (undefined :: Data.Vector.Unboxed.Vector Bool))
    ]