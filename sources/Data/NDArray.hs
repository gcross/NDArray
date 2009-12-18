-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1264:@thin NDArray.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1420:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
-- @-node:gcross.20091217190104.1420:<< Language extensions >>
-- @nl

module Data.NDArray where

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1265:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Applicative
import Control.Exception

import qualified Data.Vec as V
import Data.Vec((:.)(..))
import Data.Vec.Nat

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
class CutStrides c v vv where
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
            ((hi-lo) `div` skip)  :. cutShape cs vs
-- @nonl
-- @-node:gcross.20091217190104.1459:CutStrides
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
    assert (ndarrayContiguous ndarray) $
        withForeignPtr (ndarrayData ndarray) thunk
-- @-node:gcross.20091217190104.1275:withNDArray
-- @-node:gcross.20091217190104.1273:Pointer access
-- @+node:gcross.20091217190104.1541:fromList/toList
-- @+node:gcross.20091217190104.1537:fromList
fromList :: Storable a => [a] -> NDArray (Int :. ()) a
fromList list = fst . unsafePerformIO . withNewNDArray ((length list) :. ()) . go $ list
  where
    go [] _ = return ()
    go (x:xs) ptr = poke ptr x >> go xs (ptr `advancePtr` 1)
-- @-node:gcross.20091217190104.1537:fromList
-- @+node:gcross.20091217190104.1540:toList
toList :: Storable a => NDArray (Int :. ()) a -> [a]
toList ndarray = unsafePerformIO . withNDArray ndarray $ startGo
  where
    stride = V.head . ndarrayStrides $ ndarray
    size = V.head . ndarrayShape $ ndarray

    startGo = go [] size . (`advancePtr` (stride * (size - 1) + ndarrayBaseOffset ndarray))

    go accum 0 _ = return accum
    go accum size ptr =
        peek ptr
        >>=
        \x -> go (x:accum) (size-1) (ptr `advancePtr` (-stride))
-- @-node:gcross.20091217190104.1540:toList
-- @-node:gcross.20091217190104.1541:fromList/toList
-- @-node:gcross.20091217190104.1270:Functions
-- @-others
-- @-node:gcross.20091217190104.1264:@thin NDArray.hs
-- @-leo
