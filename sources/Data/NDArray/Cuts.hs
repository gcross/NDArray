-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1670:@thin Cuts.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1672:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- @-node:gcross.20100110123138.1672:<< Language extensions >>
-- @nl

module Data.NDArray.Cuts where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1674:<< Import needed modules >>
import Control.Exception

import Data.Vec((:.)(..))

import Data.NDArray.Indexable
-- @-node:gcross.20100110123138.1674:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1675:Types
-- @+node:gcross.20100110123138.1681:Cut Specification
-- @+node:gcross.20100110123138.1682:All
data All = All
-- @-node:gcross.20100110123138.1682:All
-- @+node:gcross.20100110123138.1683:Index
data Index = Index Int
-- @-node:gcross.20100110123138.1683:Index
-- @+node:gcross.20100110123138.1684:Range
data Range = Range Int Int
-- @-node:gcross.20100110123138.1684:Range
-- @+node:gcross.20100110123138.1685:StridedRange
data StridedRange = StridedRange Int Int Int
-- @-node:gcross.20100110123138.1685:StridedRange
-- @-node:gcross.20100110123138.1681:Cut Specification
-- @-node:gcross.20100110123138.1675:Types
-- @+node:gcross.20100110123138.1693:Classes
-- @+node:gcross.20100110123138.1687:Cut
class Cut c v where
    type CutResult c v
    cutOffset :: c -> v -> Int
    cutPreservesContiguity :: c -> v -> Bool
    cutPreservesContiguityImplementation :: c -> v -> Bool
    cutStrides :: c -> v -> CutResult c v
    cutShape :: c -> v -> CutResult c v

instance Cut () v where
    type CutResult () v = v
    cutOffset () _ = 0
    cutPreservesContiguity () _ = True
    cutPreservesContiguityImplementation () _ = True
    cutStrides () = id
    cutShape () = id

instance Cut c v => Cut (All :. c) (Int :. v) where
    type CutResult (All :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (All :. cs) (_ :. vs) = cutOffset cs vs
    cutPreservesContiguity (_ :. cs) (_ :. vs) = cutPreservesContiguityImplementation cs vs
    cutPreservesContiguityImplementation (All :. cs) (_ :. vs) = cutPreservesContiguityImplementation cs vs
    cutStrides (All :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape (All :. cs) (bound :. vs) = bound :. cutShape cs vs

instance Cut c v => Cut (Index :. c) (Int :. v) where
    type CutResult (Index :. c) (Int :. v) = CutResult c v
    cutOffset (Index index :. cs) (stride :. vs) = (index*stride) + cutOffset cs vs
    cutPreservesContiguity (_ :. cs) (_ :. vs) = cutPreservesContiguityImplementation cs vs
    cutPreservesContiguityImplementation _ _ = False
    cutStrides (_ :. cs) (_ :. vs) = cutStrides cs vs
    cutShape (Index index :. cs) (bound :. vs) =
        assert (index >= 0 || index < bound) $
            cutShape cs vs

instance Cut c v => Cut (Range :. c) (Int :. v) where
    type CutResult (Range :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (Range lo _ :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity (_ :. cs) (_ :. vs) = cutPreservesContiguityImplementation cs vs
    cutPreservesContiguityImplementation (Range lo hi :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && cutPreservesContiguityImplementation cs vs
    cutStrides (_ :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape (Range lo hi :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            (hi-lo) :. cutShape cs vs

instance Cut c v => Cut (StridedRange :. c) (Int :. v) where
    type CutResult (StridedRange :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (StridedRange lo _ _ :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity ((StridedRange _ _ skip) :. cs) (_ :. vs) =
        (skip == 1) && cutPreservesContiguityImplementation cs vs
    cutPreservesContiguityImplementation (StridedRange lo hi skip :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && (skip == 1) && cutPreservesContiguityImplementation cs vs
    cutStrides (StridedRange _ _ skip :. cs) (stride :. vs) = (skip*stride) :. cutStrides cs vs
    cutShape (StridedRange lo hi skip :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            ((hi-lo-1) `div` skip + 1)  :. cutShape cs vs
-- @-node:gcross.20100110123138.1687:Cut
-- @-node:gcross.20100110123138.1693:Classes
-- @-others
-- @-node:gcross.20100110123138.1670:@thin Cuts.hs
-- @-leo
