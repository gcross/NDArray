-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1264:@thin NDArray.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1420:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- @-node:gcross.20091217190104.1420:<< Language extensions >>
-- @nl

module Data.NDArray where

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1265:<< Import needed modules >>
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

import System.IO.Unsafe

import Test.QuickCheck.Gen

import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Descriptor
import Data.NDArray.Indexable
-- @-node:gcross.20091217190104.1265:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091217190104.1268:Types
-- @+node:gcross.20091217190104.1269:NDArray
newtype NDArray indexType dataType = NDArray { unwrapDescriptor :: Descriptor indexType dataType }
-- @-node:gcross.20091217190104.1269:NDArray
-- @+node:gcross.20091224104908.1570:ArrayND aliases
type Array1D = NDArray (Int :. ())
type Array2D = NDArray (Vec2 Int)
type Array3D = NDArray (Vec3 Int)
type Array4D = NDArray (Vec4 Int)
type Array5D = NDArray (Vec5 Int)
type Array6D = NDArray (Vec6 Int)
type Array7D = NDArray (Vec7 Int)
type Array8D = NDArray (Vec8 Int)
type Array9D = NDArray (Vec9 Int)
-- @-node:gcross.20091224104908.1570:ArrayND aliases
-- @-node:gcross.20091217190104.1268:Types
-- @+node:gcross.20100110123138.1702:Instances
-- @+node:gcross.20100110123138.1717:Accessible
instance Accessible NDArray where
    withNewNDArray shape = fmap (first NDArray) . withNewDescriptor shape
    withNDArray ndarray thunk = withDescriptor (unwrapDescriptor ndarray) thunk
    withContiguousNDArray ndarray thunk = withContiguousDescriptor (unwrapDescriptor ndarray) thunk
-- @-node:gcross.20100110123138.1717:Accessible
-- @+node:gcross.20100110123138.1713:Cutable
instance Cutable NDArray where
    cut cut_ = NDArray . cutDescriptor cut_ . unwrapDescriptor
-- @-node:gcross.20100110123138.1713:Cutable
-- @+node:gcross.20100110123138.1703:Queryable
instance Queryable (NDArray indexType dataType) where
    type QueryResultType (NDArray indexType dataType) = indexType
    ndarrayBaseOffset = descriptorBaseOffset . unwrapDescriptor
    ndarrayShape = descriptorShape . unwrapDescriptor
    ndarrayStrides = descriptorStrides . unwrapDescriptor
-- @-node:gcross.20100110123138.1703:Queryable
-- @-node:gcross.20100110123138.1702:Instances
-- @+node:gcross.20091217190104.1270:Functions
-- @+node:gcross.20091217190104.1541:fromList/toList
-- @+node:gcross.20091217190104.1537:fromList
fromList ::
    (Storable dataType) =>
    [dataType] ->
    NDArray (Int :. ()) dataType
fromList list = fromListWithShape (length list :. ()) list
-- @-node:gcross.20091217190104.1537:fromList
-- @+node:gcross.20091220115426.1652:fromListWithShape
fromListWithShape ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    [dataType] ->
    NDArray indexType dataType
fromListWithShape shape = NDArray . unsafePerformIO . descriptorFromListWithShape shape
-- @-node:gcross.20091220115426.1652:fromListWithShape
-- @+node:gcross.20091218165002.1494:toList
toList ::
    (Indexable indexType, Storable dataType) =>
    NDArray indexType dataType ->
    [dataType]
toList = unsafePerformIO . descriptorToList . unwrapDescriptor
-- @-node:gcross.20091218165002.1494:toList
-- @-node:gcross.20091217190104.1541:fromList/toList
-- @+node:gcross.20091219130644.1361:Folding
-- @+node:gcross.20091218165002.1491:foldlNDArray
foldlNDArray ::
    (Indexable indexType, Storable dataType) =>
    (a -> dataType -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldlNDArray folder seed = unsafePerformIO . foldlDescriptor folder seed . unwrapDescriptor
-- @-node:gcross.20091218165002.1491:foldlNDArray
-- @+node:gcross.20091219130644.1359:foldrNDArray
foldrNDArray ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> a -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldrNDArray folder seed = unsafePerformIO . foldrDescriptor folder seed . unwrapDescriptor
-- @-node:gcross.20091219130644.1359:foldrNDArray
-- @-node:gcross.20091219130644.1361:Folding
-- @+node:gcross.20091226065853.1616:Random
-- @+node:gcross.20091226065853.1618:arbitraryNDArray
arbitraryNDArray ::
    (Indexable indexType
    ,Storable dataType
    ) =>
    indexType ->
    Gen dataType ->
    Gen (NDArray indexType dataType)
arbitraryNDArray shape =
    fmap (fromListWithShape shape)
    .
    vectorOf (numberOfElementsFromShape shape)
-- @-node:gcross.20091226065853.1618:arbitraryNDArray
-- @-node:gcross.20091226065853.1616:Random
-- @+node:gcross.20091226102316.1370:(!)
(!) :: (Indexable indexType, Storable dataType) => NDArray indexType dataType -> indexType -> dataType
ndarray ! index = unsafePerformIO $
    withNDArray ndarray $
        peek . (`advancePtr` computeOffsetOfIndex (unwrapDescriptor ndarray) index)
-- @-node:gcross.20091226102316.1370:(!)
-- @+node:gcross.20100110123138.1711:findNDArray
findNDArray ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    NDArray indexType dataType ->
    Maybe dataType
findNDArray cond = unsafePerformIO . findDescriptor cond . unwrapDescriptor
-- @-node:gcross.20100110123138.1711:findNDArray
-- @-node:gcross.20091217190104.1270:Functions
-- @-others
-- @-node:gcross.20091217190104.1264:@thin NDArray.hs
-- @-leo
