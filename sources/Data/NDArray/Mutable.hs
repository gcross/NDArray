-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1740:@thin Mutable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1741:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- @-node:gcross.20100110123138.1741:<< Language extensions >>
-- @nl

module Data.NDArray.Mutable where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1742:<< Import needed modules >>
import Control.Arrow

import Data.Vec((:.)(..)
               ,Vec2
               ,Vec3
               ,Vec4
               ,Vec5
               ,Vec6
               ,Vec7
               ,Vec8
               ,Vec9
               )

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Test.QuickCheck.Gen

import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Descriptor
import Data.NDArray.Indexable
-- @-node:gcross.20100110123138.1742:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1743:Types
-- @+node:gcross.20100110123138.1744:NDArray
newtype MutableNDArray indexType dataType = MutableNDArray { unwrapDescriptor :: Descriptor indexType dataType }
-- @-node:gcross.20100110123138.1744:NDArray
-- @+node:gcross.20100110123138.1745:ArrayND aliases
type MutableArray1D = MutableNDArray (Int :. ())
type MutableArray2D = MutableNDArray (Vec2 Int)
type MutableArray3D = MutableNDArray (Vec3 Int)
type MutableArray4D = MutableNDArray (Vec4 Int)
type MutableArray5D = MutableNDArray (Vec5 Int)
type MutableArray6D = MutableNDArray (Vec6 Int)
type MutableArray7D = MutableNDArray (Vec7 Int)
type MutableArray8D = MutableNDArray (Vec8 Int)
type MutableArray9D = MutableNDArray (Vec9 Int)
-- @-node:gcross.20100110123138.1745:ArrayND aliases
-- @-node:gcross.20100110123138.1743:Types
-- @+node:gcross.20100110123138.1746:Instances
-- @+node:gcross.20100110123138.1747:Accessible
instance Accessible MutableNDArray where
    withNewNDArray shape = fmap (first MutableNDArray) . withNewDescriptor shape
    withNDArray ndarray thunk = withDescriptor (unwrapDescriptor ndarray) thunk
    withContiguousNDArray ndarray thunk = withContiguousDescriptor (unwrapDescriptor ndarray) thunk
-- @-node:gcross.20100110123138.1747:Accessible
-- @+node:gcross.20100110123138.1748:Cutable
instance Cutable MutableNDArray where
    cut cut_ = MutableNDArray . cutDescriptor cut_ . unwrapDescriptor
-- @-node:gcross.20100110123138.1748:Cutable
-- @+node:gcross.20100110123138.1749:Queryable
instance Queryable (MutableNDArray indexType dataType) where
    type QueryResultType (MutableNDArray indexType dataType) = indexType
    ndarrayBaseOffset = descriptorBaseOffset . unwrapDescriptor
    ndarrayShape = descriptorShape . unwrapDescriptor
    ndarrayStrides = descriptorStrides . unwrapDescriptor
-- @-node:gcross.20100110123138.1749:Queryable
-- @-node:gcross.20100110123138.1746:Instances
-- @+node:gcross.20100110123138.1750:Functions
-- @+node:gcross.20100110123138.1751:fromList/toList
-- @+node:gcross.20100110123138.1752:createFromList
createFromList ::
    (Storable dataType) =>
    [dataType] ->
    IO (MutableNDArray (Int :. ()) dataType)
createFromList list = createFromListWithShape (length list :. ()) list
-- @-node:gcross.20100110123138.1752:createFromList
-- @+node:gcross.20100110123138.1753:createFromListWithShape
createFromListWithShape ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    [dataType] ->
    IO (MutableNDArray indexType dataType)
createFromListWithShape shape = fmap MutableNDArray . descriptorFromListWithShape shape
-- @-node:gcross.20100110123138.1753:createFromListWithShape
-- @+node:gcross.20100110123138.1754:readIntoList
readIntoList ::
    (Indexable indexType, Storable dataType) =>
    MutableNDArray indexType dataType ->
    IO [dataType]
readIntoList = descriptorToList . unwrapDescriptor
-- @-node:gcross.20100110123138.1754:readIntoList
-- @-node:gcross.20100110123138.1751:fromList/toList
-- @+node:gcross.20100110123138.1755:Folding
-- @+node:gcross.20100110123138.1756:foldlMutableNDArray
foldlMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    (a -> dataType -> a) ->
    a ->
    MutableNDArray indexType dataType ->
    IO a
foldlMutableNDArray folder seed = foldlDescriptor folder seed . unwrapDescriptor
-- @-node:gcross.20100110123138.1756:foldlMutableNDArray
-- @+node:gcross.20100110123138.1757:foldrMutableNDArray
foldrMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> a -> a) ->
    a ->
    MutableNDArray indexType dataType ->
    IO a
foldrMutableNDArray folder seed = foldrDescriptor folder seed . unwrapDescriptor
-- @-node:gcross.20100110123138.1757:foldrMutableNDArray
-- @-node:gcross.20100110123138.1755:Folding
-- @+node:gcross.20100110123138.1769:Listlike
-- @+node:gcross.20100110123138.1761:findMutableNDArray
findMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    MutableNDArray indexType dataType ->
    IO (Maybe dataType)
findMutableNDArray cond = findDescriptor cond . unwrapDescriptor
-- @-node:gcross.20100110123138.1761:findMutableNDArray
-- @+node:gcross.20100110123138.1776:sum/product
sumMutableNDArray, productMutableNDArray ::
    (Indexable indexType, Storable dataType, Num dataType) =>
    MutableNDArray indexType dataType ->
    IO dataType
sumMutableNDArray = foldlMutableNDArray (+) 0
productMutableNDArray = foldlMutableNDArray (*) 1
-- @-node:gcross.20100110123138.1776:sum/product
-- @+node:gcross.20100110123138.1781:any/all/and/or
anyMutableNDArray, allMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    MutableNDArray indexType dataType ->
    IO Bool
anyMutableNDArray cond = fmap (maybe False (const True)) . findMutableNDArray cond
allMutableNDArray cond = fmap (maybe True (const False)) . findMutableNDArray (not . cond)

orMutableNDArray, andMutableNDArray ::
    Indexable indexType =>
    MutableNDArray indexType Bool ->
    IO Bool
orMutableNDArray = anyMutableNDArray id
andMutableNDArray = allMutableNDArray id
-- @-node:gcross.20100110123138.1781:any/all/and/or
-- @-node:gcross.20100110123138.1769:Listlike
-- @+node:gcross.20100110123138.1762:Element access
-- @+node:gcross.20100110123138.1764:readMutableNDArray
readMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    MutableNDArray indexType dataType ->
    indexType ->
    IO dataType
readMutableNDArray ndarray index =
    withNDArray ndarray $
        peek . (`advancePtr` computeOffsetOfIndex (unwrapDescriptor ndarray) index)
-- @-node:gcross.20100110123138.1764:readMutableNDArray
-- @+node:gcross.20100110123138.1766:writeMutableNDArray
writeMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    MutableNDArray indexType dataType ->
    indexType ->
    dataType ->
    IO ()
writeMutableNDArray ndarray index value =
    withNDArray ndarray $
        flip poke value
        .
        (`advancePtr` computeOffsetOfIndex (unwrapDescriptor ndarray) index)
-- @-node:gcross.20100110123138.1766:writeMutableNDArray
-- @+node:gcross.20100110123138.1768:modifyMutableNDArray
modifyMutableNDArray ::
    (Indexable indexType, Storable dataType) =>
    MutableNDArray indexType dataType ->
    (dataType -> dataType) ->    
    indexType ->
    IO ()
modifyMutableNDArray ndarray modifier index =
    withNDArray ndarray $ \ptr ->
        let data_ptr = ptr `advancePtr` computeOffsetOfIndex (unwrapDescriptor ndarray) index
        in  peek data_ptr
            >>=
            return . modifier
            >>=
            poke data_ptr
-- @-node:gcross.20100110123138.1768:modifyMutableNDArray
-- @-node:gcross.20100110123138.1762:Element access
-- @-node:gcross.20100110123138.1750:Functions
-- @-others
-- @-node:gcross.20100110123138.1740:@thin Mutable.hs
-- @-leo
