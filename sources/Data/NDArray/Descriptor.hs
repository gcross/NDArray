-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1445:@thin Descriptor.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1446:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
-- @-node:gcross.20100110123138.1446:<< Language extensions >>
-- @nl

module Data.NDArray.Descriptor where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1447:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Applicative.Infix
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Typeable
import Data.Vec((:.)(..))

import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Data.NDArray.Cuts
import Data.NDArray.Indexable
-- @-node:gcross.20100110123138.1447:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1453:Types
-- @+node:gcross.20100110123138.1459:Descriptor
data Descriptor indexType dataType =
    Descriptor
        {   descriptorBaseOffset :: Int
        ,   descriptorShape :: indexType
        ,   descriptorStrides :: indexType
        ,   descriptorIsContiguous :: Bool
        ,   descriptorData :: ForeignPtr dataType
        }
-- @-node:gcross.20100110123138.1459:Descriptor
-- @-node:gcross.20100110123138.1453:Types
-- @+node:gcross.20100110123138.1708:Exceptions
-- @+node:gcross.20100110123138.1709:Found
data Found a = Found a deriving (Show,Typeable)

instance (Show a, Typeable a) => Exception (Found a)
-- @-node:gcross.20100110123138.1709:Found
-- @-node:gcross.20100110123138.1708:Exceptions
-- @+node:gcross.20100110123138.1461:Functions
-- @+node:gcross.20100110123138.1462:Pointer access
-- @+node:gcross.20100110123138.1463:withNewNDArray
withNewDescriptor ::
    (Indexable indexType
    ,Storable dataType
    )=>
    indexType ->
    (Ptr dataType -> IO a) ->
    IO (Descriptor indexType dataType,a)
withNewDescriptor shape thunk = do
    foreign_ptr <- mallocForeignPtrArray . numberOfElementsFromShape $ shape
    result <- withForeignPtr foreign_ptr thunk
    return $
        (Descriptor
            {   descriptorBaseOffset = 0
            ,   descriptorShape = shape
            ,   descriptorStrides = contiguousStridesFromShape shape
            ,   descriptorIsContiguous = True
            ,   descriptorData = foreign_ptr
            }
        ,result
        )
-- @-node:gcross.20100110123138.1463:withNewNDArray
-- @+node:gcross.20100110123138.1464:withNDArray
withDescriptor ::
    Descriptor indexType dataType ->
    (Ptr dataType -> IO a) ->
    IO a
withDescriptor ndarray thunk =
    withForeignPtr (descriptorData ndarray) thunk
-- @-node:gcross.20100110123138.1464:withNDArray
-- @+node:gcross.20100110123138.1465:withContiguousNDArray
withContiguousDescriptor ::
    Descriptor indexType dataType ->
    (Ptr dataType -> IO a) ->
    IO a
withContiguousDescriptor ndarray =
    assert (descriptorIsContiguous ndarray) $
    withDescriptor ndarray
-- @-node:gcross.20100110123138.1465:withContiguousNDArray
-- @-node:gcross.20100110123138.1462:Pointer access
-- @+node:gcross.20100110123138.1466:cutDescriptor
cutDescriptor ::
    (Cut cut oldIndexType) =>
    cut ->
    Descriptor oldIndexType dataType ->
    Descriptor (CutResult cut oldIndexType) dataType
cutDescriptor cut_ =
    Descriptor
        <$> (cutOffset cut_ . descriptorStrides <^(+)^> descriptorBaseOffset)
        <*> (cutShape cut_ . descriptorShape)
        <*> (cutStrides cut_ . descriptorStrides)
        <*> (descriptorIsContiguous <^(&&)^> cutPreservesContiguity cut_ . descriptorShape)
        <*> descriptorData
-- @-node:gcross.20100110123138.1466:cutDescriptor
-- @+node:gcross.20100110123138.1467:fromList/toList
-- @+node:gcross.20100110123138.1468:descriptorFromList
descriptorFromList ::
    (Storable dataType) =>
    [dataType] ->
    IO (Descriptor (Int :. ()) dataType)
descriptorFromList list = descriptorFromListWithShape (length list :. ()) list
-- @-node:gcross.20100110123138.1468:descriptorFromList
-- @+node:gcross.20100110123138.1469:descriptorFromListWithShape
descriptorFromListWithShape ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    [dataType] ->
    IO (Descriptor indexType dataType)
descriptorFromListWithShape shape =
    fmap fst
    .
    withNewDescriptor shape
    .
    go (numberOfElementsFromShape shape)
  where
    go 0 _ _ = return ()
    go n [] _ = error ("Ran out of list elements while populating array; expected " ++ show (numberOfElementsFromShape shape) ++ " but saw " ++ show (numberOfElementsFromShape shape - n))
    go n (x:xs) ptr = poke ptr x >> go (n-1) xs (ptr `advancePtr` 1)
-- @-node:gcross.20100110123138.1469:descriptorFromListWithShape
-- @+node:gcross.20100110123138.1470:descriptorToList
descriptorToList ::
    (Indexable indexType, Storable dataType) =>
    Descriptor indexType dataType ->
    IO [dataType]
descriptorToList = foldrDescriptor (:) []
-- @-node:gcross.20100110123138.1470:descriptorToList
-- @-node:gcross.20100110123138.1467:fromList/toList
-- @+node:gcross.20100110123138.1471:Special case walks
-- @+node:gcross.20100110123138.1472:reverseWalk
reverseWalk ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    indexType ->
    (Ptr dataType -> a -> IO a) ->
    Ptr dataType ->
    a ->
    IO a
reverseWalk shape strides thunk ptr =
    walk
        shape
        (reversedStrides strides)
        thunk
        (ptr `advancePtr` lastOffset shape strides)
-- @-node:gcross.20100110123138.1472:reverseWalk
-- @+node:gcross.20100110123138.1473:fastReverseWalk
fastReverseWalk ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    (Ptr dataType -> a -> IO a) ->
    Ptr dataType ->
    a ->
    IO a
fastReverseWalk shape thunk ptr =
    walk
        (size :. ())
        (-1 :. ())
        thunk
        (ptr `advancePtr` (size-1))
  where
    size = numberOfElementsFromShape shape
-- @-node:gcross.20100110123138.1473:fastReverseWalk
-- @+node:gcross.20100110123138.1474:fastWalk
fastWalk ::
    (Indexable indexType, Storable dataType) =>
    indexType ->
    (Ptr dataType -> a -> IO a) ->
    Ptr dataType ->
    a ->
    IO a
fastWalk shape thunk =
    walk
        (size :. ())
        (1 :. ())
        thunk
  where
    size = numberOfElementsFromShape shape
-- @-node:gcross.20100110123138.1474:fastWalk
-- @-node:gcross.20100110123138.1471:Special case walks
-- @+node:gcross.20100110123138.1475:Folding
-- @+node:gcross.20100110123138.1476:foldlDescriptor
foldlDescriptor ::
    (Indexable indexType, Storable dataType) =>
    (a -> dataType -> a) ->
    a ->
    Descriptor indexType dataType ->
    IO a
foldlDescriptor folder seed ndarray =
    withDescriptor ndarray
    $
    \ptr ->
        (if descriptorIsContiguous ndarray
            then
                fastWalk
                    (descriptorShape ndarray)
            else
                walk
                    (descriptorShape ndarray)
                    (descriptorStrides ndarray)
        )
            thunk
            (ptr `advancePtr` descriptorBaseOffset ndarray)
            seed
  where
    thunk ptr accum = peek ptr >>= evaluate . folder accum
-- @-node:gcross.20100110123138.1476:foldlDescriptor
-- @+node:gcross.20100110123138.1477:foldrDescriptor
foldrDescriptor ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> a -> a) ->
    a ->
    Descriptor indexType dataType ->
    IO a
foldrDescriptor folder seed ndarray =
    withDescriptor ndarray
    $
    \ptr ->
        (if descriptorIsContiguous ndarray
            then
                fastReverseWalk
                    (descriptorShape ndarray)
            else
                reverseWalk
                    (descriptorShape ndarray)
                    (descriptorStrides ndarray)
        )
            thunk
            (ptr `advancePtr` descriptorBaseOffset ndarray)
            seed
  where
    thunk ptr accum = peek ptr >>= evaluate . flip folder accum
-- @-node:gcross.20100110123138.1477:foldrDescriptor
-- @-node:gcross.20100110123138.1475:Folding
-- @+node:gcross.20100110123138.1481:computeOffsetOfIndex
computeOffsetOfIndex :: Indexable indexType => Descriptor indexType dataType -> indexType -> Int
computeOffsetOfIndex ndarray index = descriptorBaseOffset ndarray + offsetUsingStrides (descriptorStrides ndarray) index
-- @-node:gcross.20100110123138.1481:computeOffsetOfIndex
-- @+node:gcross.20100110123138.1705:findDescriptor
findDescriptor ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    Descriptor indexType dataType ->
    IO (Maybe dataType)
findDescriptor cond ndarray =
    withDescriptor ndarray
    $
    \ptr ->
        (
            (if descriptorIsContiguous ndarray
                then
                    fastWalk
                        (descriptorShape ndarray)
                else
                    walk
                        (descriptorShape ndarray)
                        (descriptorStrides ndarray)
            )
                thunk
                (ptr `advancePtr` descriptorBaseOffset ndarray)
                ()
            >>
            return Nothing
        ) `catch` (
            \(Found location) -> fmap Just (peek . wordPtrToPtr $ location)
        )

  where
    thunk ptr _ =
        peek ptr
        >>=
        \value ->
            if cond value
                then throw (Found . ptrToWordPtr $ ptr)
                else return ()
-- @-node:gcross.20100110123138.1705:findDescriptor
-- @-node:gcross.20100110123138.1461:Functions
-- @-others
-- @-node:gcross.20100110123138.1445:@thin Descriptor.hs
-- @-leo
