-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1264:@thin NDArray.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1420:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
