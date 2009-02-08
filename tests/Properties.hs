{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Properties (tests) where

import Utilities

import qualified Data.Vector.IVector as V
import qualified Data.Vector
import qualified Data.Vector.Unboxed

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck

import Text.Show.Functions
import Data.List (foldl', foldl1', unfoldr)

testVersusLists :: forall a v.
                    (Eq a,     Ord a,
                     Eq (v a), Ord (v a),
                     Show a,        Arbitrary a,        Model a a,
                     -- This would be slightly nicer if we could put forall quantifiers in the class requirements!
                     Show (v a),    Arbitrary (v a),    Model (v a) [a],       V.IVector v a,
                     Show (v Bool), Arbitrary (v Bool), Model (v Bool) [Bool], V.IVector v Bool)
                     => v a
                     -> [Test]
testVersusLists _ = [
        testGroup "Prelude"   prelude_tests,
        testGroup "Data.List" data_list_tests,
        testGroup "Extras"    extra_tests
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
            testProperty "(:)"          prop_cons,
            testProperty "drop"         prop_drop,
            testProperty "dropWhile"    prop_dropWhile,
            testProperty "take"         prop_take,
            testProperty "takeWhile"    prop_takeWhile,
            testProperty "filter"       prop_filter,
            testProperty "map"          prop_map,
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
            testProperty "unfoldr"      prop_unfoldr,
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
    prop_cons         = (V.cons :: a -> v a -> v a)                   `eq2` (:)
    prop_drop         = (V.drop :: Int -> v a -> v a)                 `eq2` drop
    prop_dropWhile    = (V.dropWhile :: (a -> Bool) -> v a -> v a)    `eq2` dropWhile
    prop_take         = (V.take :: Int -> v a -> v a)                 `eq2` take
    prop_takeWhile    = (V.takeWhile :: (a -> Bool) -> v a -> v a)    `eq2` takeWhile
    prop_filter       = (V.filter :: (a -> Bool) -> v a -> v a)       `eq2` filter
    prop_map          = (V.map :: (a -> a) -> v a -> v a)             `eq2` map
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
    prop_eq           = ((==) :: v a -> v a -> Bool) `eq2` (==)
    prop_compare      = (compare :: v a -> v a -> Ordering)           `eq2` compare
    
    data_list_tests = [
            testProperty "foldl'"       prop_foldl',
            testProperty "foldl1'"      prop_foldl1',
            testProperty "unfoldr"      prop_unfoldr
            --testProperty "transpose"    prop_transpose,
            --testProperty "group"        prop_group,
            --testProperty "inits"        prop_inits,
            --testProperty "tails"        prop_tails,
            --testProperty "find"         prop_find,
            --testProperty "findIndices"  prop_findIndices,
            --testProperty "findIndex"    prop_findIndex,
            --testProperty "isPrefixOf"   prop_isPrefixOf,
            --testProperty "elemIndex"    prop_elemIndex,
            --testProperty "elemIndices"  prop_elemIndices,
            --testProperty "mapAccumL"    prop_mapAccumL,
            --testProperty "mapAccumR"    prop_mapAccumR,
        ]
    
    -- TODO: implement Vector equivalents for some of the commented out list functions from Data.List
    prop_foldl'       = (V.foldl' :: (a -> a -> a) -> a -> v a -> a)  `eq3` foldl'
    prop_foldl1'      = (V.foldl1' :: (a -> a -> a) -> v a -> a)      `eqNotNull2` foldl1'
    prop_unfoldr      = ((\n f a -> V.take n $ V.unfoldr f a) :: Int -> (Int -> Maybe (a, Int)) -> Int -> v a)
                        `eq3` (\n f a -> take n   $ unfoldr f a)
    --prop_transpose    = V.transpose   `eq1` (transpose   :: [v a] -> [v a])
    --prop_group        = V.group       `eq1` (group       :: v a -> [v a])
    --prop_inits        = V.inits       `eq1` (inits       :: v a -> [v a])
    --prop_tails        = V.tails       `eq1` (tails       :: v a -> [v a])
    --prop_find         = V.find        `eq2` (find        :: (a -> Bool) -> v a -> Maybe a)
    --prop_findIndices  = V.findIndices `eq2` (findIndices :: (a -> Bool) -> v a -> [Int])
    --prop_findIndex    = V.findIndex   `eq2` (findIndex   :: (a -> Bool) -> v a -> Maybe Int)
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

    extra_tests = [
            testProperty "snoc"         prop_snoc
        ]

    -- TODO: add tests for the other extra functions
    snoc xs x = xs ++ [x]
    prop_snoc = (V.snoc :: v a -> a -> v a)                   `eq2` snoc

tests = [
        testGroup "Data.Vector.Vector"                (testVersusLists (undefined :: Data.Vector.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Int)"  (testVersusLists (undefined :: Data.Vector.Unboxed.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Bool)" (testVersusLists (undefined :: Data.Vector.Unboxed.Vector Bool))
    ]