-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1264:@thin NDArray.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1420:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- @-node:gcross.20091217190104.1420:<< Language extensions >>
-- @nl

module Data.NDArray where

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1265:<< Import needed modules >>
import Prelude hiding (foldl,foldr)

import Control.Applicative.Infix
import Control.Applicative
import Control.Exception
import Control.Monad

import qualified Data.Vec as V
import Data.Vec((:.)(..))
import Data.Vec.Nat
import Data.Vec.LinAlg

import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe
-- @-node:gcross.20091217190104.1265:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091217190104.1266:Classes
-- @+node:gcross.20091217190104.1267:Stridable
class Stridable v where
    contiguousStridesFromShape :: v -> v

instance Num a => Stridable (a :. ()) where
   contiguousStridesFromShape _ = 1 :. ()

instance (Num a, Stridable (a :. u)) => Stridable (a :. (a :. u)) where
    contiguousStridesFromShape (x :. xs@(y :. ys)) =
        let rest_strides@(stride :. _) = contiguousStridesFromShape xs
        in (stride * y) :. rest_strides
-- @-node:gcross.20091217190104.1267:Stridable
-- @+node:gcross.20091217190104.1427:CutOffset
class CutOffset c v where
    cutOffset :: c -> v -> Int
    cutPreservesContiguity :: c -> v -> Bool

instance CutOffset () () where
    cutOffset () _ = 0
    cutPreservesContiguity () _ = True

instance CutOffset c v => CutOffset (() :. c) (Int :. v) where
    cutOffset (() :. cs) (_ :. vs) = cutOffset cs vs
    cutPreservesContiguity (() :. cs) (_ :. vs) = cutPreservesContiguity cs vs

instance CutOffset c v => CutOffset (Int :. c) (Int :. v) where
    cutOffset (index :. cs) (stride :. vs) = (index*stride) + cutOffset cs vs
    cutPreservesContiguity _ _ = False

instance CutOffset c v => CutOffset ((Int,Int) :. c) (Int :. v) where
    cutOffset ((lo,_) :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity ((lo,hi) :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && cutPreservesContiguity cs vs

instance CutOffset c v => CutOffset ((Int,Int,Int) :. c) (Int :. v) where
    cutOffset ((lo,_,_) :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity ((lo,hi,skip) :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && (skip == 1) && cutPreservesContiguity cs vs
-- @-node:gcross.20091217190104.1427:CutOffset
-- @+node:gcross.20091217190104.1459:CutStrides
class CutStrides c v vv | c v -> vv where
    cutStrides :: c -> v -> vv
    cutShape :: c -> v -> vv

instance CutStrides () () () where
    cutStrides () = id
    cutShape () = id

instance CutStrides c v vv => CutStrides (() :. c) (Int :. v) (Int :. vv) where
    cutStrides (() :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape (() :. cs) (bound :. vs) = bound :. cutShape cs vs

instance CutStrides c v vv => CutStrides (Int :. c) (Int :. v) vv where
    cutStrides (_ :. cs) (_ :. vs) = cutStrides cs vs
    cutShape (index :. cs) (bound :. vs) =
        assert (index >= 0 || index < bound) $
            cutShape cs vs

instance CutStrides c v vv => CutStrides ((Int,Int) :. c) (Int :. v) (Int :. vv) where
    cutStrides (_ :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape ((lo,hi) :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            (hi-lo) :. cutShape cs vs

instance CutStrides c v vv => CutStrides ((Int,Int,Int) :. c) (Int :. v) (Int :. vv) where
    cutStrides ((_,_,skip) :. cs) (stride :. vs) = (skip*stride) :. cutStrides cs vs
    cutShape ((lo,hi,skip) :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            ((hi-lo-1) `div` skip + 1)  :. cutShape cs vs
-- @-node:gcross.20091217190104.1459:CutStrides
-- @+node:gcross.20091218165002.1490:Indexable
class Indexable indexType where
    walk ::
        Storable dataType =>
        indexType ->
        indexType ->
        (Ptr dataType -> a -> IO a) ->
        Ptr dataType ->
        a ->
        IO a
    lastOffset :: indexType -> indexType -> Int
    reversedStrides :: indexType -> indexType
    numberOfElementsFromShape :: indexType -> Int

instance Indexable () where
    walk _ _ thunk = thunk
    lastOffset _ _ = 0
    reversedStrides = id
    numberOfElementsFromShape _ = 1

instance Indexable a => Indexable (Int :. a) where
    walk
        (size :. rest_shape)
        (stride :. rest_strides)
        thunk
        = go size stride
      where
        go 0 _ _ = return
        go size stride ptr =
            walk rest_shape rest_strides thunk ptr
            >=>
            go (size-1) stride (ptr `advancePtr` stride)

    lastOffset (shape :. rest_shape) (stride :. rest_strides) =
        (shape-1)*stride + lastOffset rest_shape rest_strides

    reversedStrides (stride :. rest_strides) = (-stride) :. reversedStrides rest_strides

    numberOfElementsFromShape (shape :. rest_shape) = shape * numberOfElementsFromShape rest_shape
-- @-node:gcross.20091218165002.1490:Indexable
-- @-node:gcross.20091217190104.1266:Classes
-- @+node:gcross.20091217190104.1268:Types
-- @+node:gcross.20091217190104.1269:NDArray
data NDArray indexType dataType =
    NDArray
        {   ndarrayBaseOffset :: Int
        ,   ndarrayShape :: indexType
        ,   ndarrayStrides :: indexType
        ,   ndarrayContiguous :: Bool
        ,   ndarrayData :: ForeignPtr dataType
        }
-- @-node:gcross.20091217190104.1269:NDArray
-- @-node:gcross.20091217190104.1268:Types
-- @+node:gcross.20091217190104.1270:Functions
-- @+node:gcross.20091217190104.1273:Pointer access
-- @+node:gcross.20091217190104.1274:withNewNDArray
withNewNDArray ::
    (V.Fold indexType Int
    ,Stridable indexType
    ,Storable dataType
    )=>
    indexType ->
    (Ptr dataType -> IO a) ->
    IO (NDArray indexType dataType,a)
withNewNDArray bounds thunk = do
    foreign_ptr <- mallocForeignPtrArray (V.product bounds)
    result <- withForeignPtr foreign_ptr thunk
    return $
        (NDArray
            {   ndarrayBaseOffset = 0
            ,   ndarrayShape = bounds
            ,   ndarrayStrides = contiguousStridesFromShape bounds
            ,   ndarrayContiguous = True
            ,   ndarrayData = foreign_ptr
            }
        ,result
        )
-- @-node:gcross.20091217190104.1274:withNewNDArray
-- @+node:gcross.20091217190104.1275:withNDArray
withNDArray ::
    NDArray indexType dataType ->
    (Ptr dataType -> IO a) ->
    IO a
withNDArray ndarray thunk =
    withForeignPtr (ndarrayData ndarray) thunk
-- @-node:gcross.20091217190104.1275:withNDArray
-- @-node:gcross.20091217190104.1273:Pointer access
-- @+node:gcross.20091217190104.1536:cut
cut ::
    (CutOffset cut oldIndexType, CutStrides cut oldIndexType newIndexType) =>
    cut ->
    NDArray oldIndexType dataType ->
    NDArray newIndexType dataType
cut cut_ =
    NDArray
        <$> (cutOffset cut_ . ndarrayStrides <^(+)^> ndarrayBaseOffset)
        <*> (cutShape cut_ . ndarrayShape)
        <*> (cutStrides cut_ . ndarrayStrides)
        <*> (cutPreservesContiguity cut_ . ndarrayShape <^(&&)^> ndarrayContiguous)
        <*> ndarrayData
-- @-node:gcross.20091217190104.1536:cut
-- @+node:gcross.20091217190104.1541:fromList/toList
-- @+node:gcross.20091217190104.1537:fromList
fromList :: Storable a => [a] -> NDArray (Int :. ()) a
fromList list = fst . unsafePerformIO . withNewNDArray ((length list) :. ()) . go $ list
  where
    go [] _ = return ()
    go (x:xs) ptr = poke ptr x >> go xs (ptr `advancePtr` 1)
-- @-node:gcross.20091217190104.1537:fromList
-- @+node:gcross.20091218165002.1494:toList
toList ::
    (Indexable indexType, Storable dataType) =>
    NDArray indexType dataType ->
    [dataType]
toList = foldr (:) []
-- @-node:gcross.20091218165002.1494:toList
-- @-node:gcross.20091217190104.1541:fromList/toList
-- @+node:gcross.20091219130644.1360:Special case walks
-- @+node:gcross.20091218165002.1492:reverseWalk
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
-- @-node:gcross.20091218165002.1492:reverseWalk
-- @+node:gcross.20091218165002.1496:fastReverseWalk
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
-- @-node:gcross.20091218165002.1496:fastReverseWalk
-- @+node:gcross.20091219130644.1357:fastWalk
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
-- @-node:gcross.20091219130644.1357:fastWalk
-- @-node:gcross.20091219130644.1360:Special case walks
-- @+node:gcross.20091219130644.1361:Folding
-- @+node:gcross.20091218165002.1491:foldl
foldl ::
    (Indexable indexType, Storable dataType) =>
    (a -> dataType -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldl folder seed ndarray =
    unsafePerformIO
    .
    withNDArray ndarray
    $
    \ptr ->
        (if ndarrayContiguous ndarray
            then
                fastWalk
                    (ndarrayShape ndarray)
            else
                walk
                    (ndarrayShape ndarray)
                    (ndarrayStrides ndarray)
        )
            thunk
            (ptr `advancePtr` ndarrayBaseOffset ndarray)
            seed
  where
    thunk ptr accum = peek ptr >>= evaluate . folder accum
-- @-node:gcross.20091218165002.1491:foldl
-- @+node:gcross.20091219130644.1359:foldr
foldr ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> a -> a) ->
    a ->
    NDArray indexType dataType ->
    a
foldr folder seed ndarray =
    unsafePerformIO
    .
    withNDArray ndarray
    $
    \ptr ->
        (if ndarrayContiguous ndarray
            then
                fastReverseWalk
                    (ndarrayShape ndarray)
            else
                reverseWalk
                    (ndarrayShape ndarray)
                    (ndarrayStrides ndarray)
        )
            thunk
            (ptr `advancePtr` ndarrayBaseOffset ndarray)
            seed
  where
    thunk ptr accum = peek ptr >>= evaluate . flip folder accum
-- @-node:gcross.20091219130644.1359:foldr
-- @+node:gcross.20091219130644.1362:sum/product
sum, product ::
    (Indexable indexType, Storable dataType, Num dataType) =>
    NDArray indexType dataType ->
    dataType
sum = foldl (+) 0
product = foldl (*) 1
-- @-node:gcross.20091219130644.1362:sum/product
-- @-node:gcross.20091219130644.1361:Folding
-- @-node:gcross.20091217190104.1270:Functions
-- @-others
-- @-node:gcross.20091217190104.1264:@thin NDArray.hs
-- @-leo
