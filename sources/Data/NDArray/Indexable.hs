-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1688:@thin Indexable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1695:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
-- @-node:gcross.20100110123138.1695:<< Language extensions >>
-- @nl

module Data.NDArray.Indexable where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1694:<< Import needed modules >>
import Control.Monad

import qualified Data.Vec as V
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
import Data.Vec.LinAlg

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
-- @nonl
-- @-node:gcross.20100110123138.1694:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1690:Indexable
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
    contiguousStridesFromShape :: indexType -> indexType
    _computeNextStride :: indexType -> indexType -> Int
    offsetUsingStrides :: indexType -> indexType -> Int

instance Indexable () where
    walk () () thunk = thunk
    lastOffset () () = 0
    reversedStrides = id
    numberOfElementsFromShape () = 1
    contiguousStridesFromShape () = ()
    _computeNextStride () () = 1
    offsetUsingStrides () () = 0

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
    contiguousStridesFromShape (x :. rest_shape) =
        let rest_strides = contiguousStridesFromShape rest_shape
        in _computeNextStride rest_shape rest_strides :. rest_strides
    _computeNextStride (shape :. _) (stride :. _) = shape*stride
    offsetUsingStrides (index :. rest_index) (stride :. rest_strides) = index*stride + offsetUsingStrides rest_index rest_strides
-- @-node:gcross.20100110123138.1690:Indexable
-- @+node:gcross.20100110123138.1692:iN / shapeN
i0 = ()
i1 a = a :. () :: Int :. ()
i2 a b = a :. b :. () :: Vec2 Int
i3 a b c = a :. b :. c :. () :: Vec3 Int
i4 a b c d = a :. b :. c :. d :. () :: Vec4 Int
i5 a b c d e = a :. b :. c :. d :. e :. () :: Vec5 Int
i6 a b c d e f = a :. b :. c :. d :. e :. f :. () :: Vec6 Int
i7 a b c d e f g = a :. b :. c :. d :. e :. f :. g :. () :: Vec7 Int
i8 a b c d e f g h = a :. b :. c :. d :. e :. f :. g :. h :. () :: Vec8 Int
i9 a b c d e f g h i = a :. b :. c :. d :. e :. f :. g :. h :. i :. () :: Vec9 Int

shape0 = i0
shape1 = i1
shape2 = i2
shape3 = i3
shape4 = i4
shape5 = i5
shape6 = i6
shape7 = i7
shape8 = i8
shape9 = i9
-- @-node:gcross.20100110123138.1692:iN / shapeN
-- @-others
-- @-node:gcross.20100110123138.1688:@thin Indexable.hs
-- @-leo
