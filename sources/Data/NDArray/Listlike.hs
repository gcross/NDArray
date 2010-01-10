-- @+leo-ver=4-thin
-- @+node:gcross.20091226065853.1785:@thin Listlike.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091226065853.1786:<< Language extensions >>
-- @-node:gcross.20091226065853.1786:<< Language extensions >>
-- @nl

module Data.NDArray.Listlike where

-- @<< Import needed modules >>
-- @+node:gcross.20091226065853.1787:<< Import needed modules >>
import Prelude hiding (foldl,foldr,catch,any,all,and,or)

import Control.Exception

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Data.NDArray
import Data.NDArray.Descriptor
import Data.NDArray.Indexable
import Data.NDArray.Queryable
-- @-node:gcross.20091226065853.1787:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091226065853.1788:Functions
-- @+node:gcross.20091226065853.1795:Folding
-- @+node:gcross.20091226065853.1796:foldl
foldl ::
    (Indexable indexType, Storable dataType) =>
    (a -> dataType -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldl = foldlNDArray
-- @-node:gcross.20091226065853.1796:foldl
-- @+node:gcross.20091226065853.1797:foldr
foldr ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> a -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldr = foldrNDArray
-- @-node:gcross.20091226065853.1797:foldr
-- @+node:gcross.20091226065853.1798:sum/product
sum, product ::
    (Indexable indexType, Storable dataType, Num dataType) =>
    NDArray indexType dataType ->
    dataType
sum = foldl (+) 0
product = foldl (*) 1
-- @-node:gcross.20091226065853.1798:sum/product
-- @+node:gcross.20091226065853.1800:any/all/and/or
any, all ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    NDArray indexType dataType ->
    Bool
any cond = maybe False (const True) . find cond
all cond = maybe True (const False) . find (not . cond)

or, and :: Indexable indexType => NDArray indexType Bool -> Bool
or = any id
and = all id
-- @-node:gcross.20091226065853.1800:any/all/and/or
-- @+node:gcross.20100110123138.1710:find
find ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    NDArray indexType dataType ->
    Maybe dataType
find = findNDArray
-- @-node:gcross.20100110123138.1710:find
-- @-node:gcross.20091226065853.1795:Folding
-- @-node:gcross.20091226065853.1788:Functions
-- @-others
-- @nonl
-- @-node:gcross.20091226065853.1785:@thin Listlike.hs
-- @-leo
