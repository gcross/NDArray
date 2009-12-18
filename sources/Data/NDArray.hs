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
import Control.Exception

import qualified Data.Vec as V
import Data.Vec((:.)(..))
import Data.Vec.Nat

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
-- @-node:gcross.20091217190104.1265:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091217190104.1266:Classes
-- @+node:gcross.20091217190104.1267:Stridable
class Stridable v where
    contiguousStridesFromShape :: v -> v

instance Stridable (a :. ()) where
   contiguousStridesFromShape = id

instance (Num a, Stridable (a :. u)) => Stridable (a :. (a :. u)) where
    contiguousStridesFromShape (x :. xs) =
        let xs_strides@(y :. _) = contiguousStridesFromShape xs
        in (x * y) :. xs_strides
-- @-node:gcross.20091217190104.1267:Stridable
-- @+node:gcross.20091217190104.1427:CutOffset
class CutOffset c v where
    cutOffset :: c -> v -> Int

instance CutOffset () () where
    cutOffset () _ = 0

instance CutOffset c v => CutOffset (() :. c) (Int :. v) where
    cutOffset (() :. cs) (_ :. vs) = cutOffset cs vs

instance CutOffset c v => CutOffset (Int :. c) (Int :. v) where
    cutOffset (index :. cs) (stride :. vs) = (index*stride) + cutOffset cs vs

instance CutOffset c v => CutOffset ((Int,Int) :. c) (Int :. v) where
    cutOffset ((lower,_) :. cs) (stride :. vs) = (lower*stride) + cutOffset cs vs

instance CutOffset c v => CutOffset ((Int,Int,Int) :. c) (Int :. v) where
    cutOffset ((lower,_,_) :. cs) (stride :. vs) = (lower*stride) + cutOffset cs vs
-- @-node:gcross.20091217190104.1427:CutOffset
-- @+node:gcross.20091217190104.1459:CutStrides
class CutStrides c v vv where
    cutStrides :: c -> v -> vv
    cutBounds :: c -> v -> vv

instance CutStrides () () () where
    cutStrides () = id
    cutBounds () = id

instance CutStrides c v vv => CutStrides (() :. c) (Int :. v) (Int :. vv) where
    cutStrides (() :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutBounds (() :. cs) (bound :. vs) = bound :. cutBounds cs vs

instance CutStrides c v vv => CutStrides (Int :. c) (Int :. v) vv where
    cutStrides (_ :. cs) (_ :. vs) = cutStrides cs vs
    cutBounds (index :. cs) (bound :. vs) =
        assert (index >= 0 || index < bound) $
            cutBounds cs vs

instance CutStrides c v vv => CutStrides ((Int,Int) :. c) (Int :. v) (Int :. vv) where
    cutStrides (_ :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutBounds ((lo,hi) :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            (hi-lo) :. cutBounds cs vs

instance CutStrides c v vv => CutStrides ((Int,Int,Int) :. c) (Int :. v) (Int :. vv) where
    cutStrides ((_,_,skip) :. cs) (stride :. vs) = (skip*stride) :. cutStrides cs vs
    cutBounds ((lo,hi,skip) :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            ((hi-lo) `div` skip)  :. cutBounds cs vs
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
-- @-node:gcross.20091217190104.1270:Functions
-- @-others
-- @-node:gcross.20091217190104.1264:@thin NDArray.hs
-- @-leo
