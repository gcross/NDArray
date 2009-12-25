-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1264:@thin NDArray.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1420:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- @-node:gcross.20091217190104.1420:<< Language extensions >>
-- @nl

module Data.NDArray where

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1265:<< Import needed modules >>
import Prelude hiding (foldl,foldr,catch,any,all,and,or)

import Control.Applicative.Infix
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Typeable
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
-- @+node:gcross.20091217190104.1459:Cut
class Cut c v where
    type CutResult c v
    cutOffset :: c -> v -> Int
    cutPreservesContiguity :: c -> v -> Bool
    cutStrides :: c -> v -> CutResult c v
    cutShape :: c -> v -> CutResult c v

instance Cut () () where
    type CutResult () () = ()
    cutOffset () _ = 0
    cutPreservesContiguity () _ = True
    cutStrides () = id
    cutShape () = id

instance Cut c v => Cut (All :. c) (Int :. v) where
    type CutResult (All :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (All :. cs) (_ :. vs) = cutOffset cs vs
    cutPreservesContiguity (All :. cs) (_ :. vs) = cutPreservesContiguity cs vs
    cutStrides (All :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape (All :. cs) (bound :. vs) = bound :. cutShape cs vs

instance Cut c v => Cut (Index :. c) (Int :. v) where
    type CutResult (Index :. c) (Int :. v) = CutResult c v
    cutOffset (Index index :. cs) (stride :. vs) = (index*stride) + cutOffset cs vs
    cutPreservesContiguity _ _ = False
    cutStrides (_ :. cs) (_ :. vs) = cutStrides cs vs
    cutShape (Index index :. cs) (bound :. vs) =
        assert (index >= 0 || index < bound) $
            cutShape cs vs

instance Cut c v => Cut (Range :. c) (Int :. v) where
    type CutResult (Range :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (Range lo _ :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity (Range lo hi :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && cutPreservesContiguity cs vs
    cutStrides (_ :. cs) (stride :. vs) = stride :. cutStrides cs vs
    cutShape (Range lo hi :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            (hi-lo) :. cutShape cs vs

instance Cut c v => Cut (StridedRange :. c) (Int :. v) where
    type CutResult (StridedRange :. c) (Int :. v) = Int :. (CutResult c v)
    cutOffset (StridedRange lo _ _ :. cs) (stride :. vs) = (lo*stride) + cutOffset cs vs
    cutPreservesContiguity (StridedRange lo hi skip :. cs) (bound :. vs) =
        (lo == 0) && (hi == bound) && (skip == 1) && cutPreservesContiguity cs vs
    cutStrides (StridedRange _ _ skip :. cs) (stride :. vs) = (skip*stride) :. cutStrides cs vs
    cutShape (StridedRange lo hi skip :. cs) (bound :. vs) =
        assert (lo >= 0 || hi < bound) $
            ((hi-lo-1) `div` skip + 1)  :. cutShape cs vs
-- @-node:gcross.20091217190104.1459:Cut
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
    contiguousStridesFromShape :: indexType -> indexType
    _computeNextStride :: indexType -> indexType -> Int

instance Indexable () where
    walk _ _ thunk = thunk
    lastOffset _ _ = 0
    reversedStrides = id
    numberOfElementsFromShape _ = 1
    contiguousStridesFromShape _ = ()
    _computeNextStride _ _ = 1

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
-- @-node:gcross.20091218165002.1490:Indexable
-- @-node:gcross.20091217190104.1266:Classes
-- @+node:gcross.20091219130644.1371:Exceptions
-- @+node:gcross.20091219130644.1372:Found
data Found a = Found a deriving (Show,Typeable)

instance (Show a, Typeable a) => Exception (Found a)
-- @-node:gcross.20091219130644.1372:Found
-- @-node:gcross.20091219130644.1371:Exceptions
-- @+node:gcross.20091217190104.1268:Types
-- @+node:gcross.20091224210553.1380:Cut Specification
-- @+node:gcross.20091224210553.1381:All
data All = All
-- @-node:gcross.20091224210553.1381:All
-- @+node:gcross.20091224210553.1382:Index
data Index = Index Int
-- @-node:gcross.20091224210553.1382:Index
-- @+node:gcross.20091224210553.1383:Range
data Range = Range Int Int
-- @-node:gcross.20091224210553.1383:Range
-- @+node:gcross.20091224210553.1384:StridedRange
data StridedRange = StridedRange Int Int Int
-- @-node:gcross.20091224210553.1384:StridedRange
-- @-node:gcross.20091224210553.1380:Cut Specification
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
-- @+node:gcross.20091224104908.1570:_NDArray
type Array1D = NDArray (Int :. ())
type Array2D = NDArray (Vec2 Int)
type Array3D = NDArray (Vec3 Int)
type Array4D = NDArray (Vec4 Int)
type Array5D = NDArray (Vec5 Int)
type Array6D = NDArray (Vec6 Int)
type Array7D = NDArray (Vec7 Int)
type Array8D = NDArray (Vec8 Int)
type Array9D = NDArray (Vec9 Int)
-- @-node:gcross.20091224104908.1570:_NDArray
-- @-node:gcross.20091217190104.1268:Types
-- @+node:gcross.20091217190104.1270:Functions
-- @+node:gcross.20091217190104.1273:Pointer access
-- @+node:gcross.20091217190104.1274:withNewNDArray
withNewNDArray ::
    (Indexable indexType
    ,Storable dataType
    )=>
    indexType ->
    (Ptr dataType -> IO a) ->
    IO (NDArray indexType dataType,a)
withNewNDArray shape thunk = do
    foreign_ptr <- mallocForeignPtrArray . numberOfElementsFromShape $ shape
    result <- withForeignPtr foreign_ptr thunk
    return $
        (NDArray
            {   ndarrayBaseOffset = 0
            ,   ndarrayShape = shape
            ,   ndarrayStrides = contiguousStridesFromShape shape
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
    (Cut cut oldIndexType) =>
    cut ->
    NDArray oldIndexType dataType ->
    NDArray (CutResult cut oldIndexType) dataType
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
fromListWithShape shape list =
    assert (numberOfElementsFromShape shape == length list) $
        fst . unsafePerformIO . withNewNDArray shape . go $ list
  where
    go [] _ = return ()
    go (x:xs) ptr = poke ptr x >> go xs (ptr `advancePtr` 1)
-- @-node:gcross.20091220115426.1652:fromListWithShape
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
-- @+node:gcross.20091219130644.1370:find
find ::
    (Indexable indexType, Storable dataType) =>
    (dataType -> Bool) ->
    NDArray indexType dataType ->
    Maybe dataType
find cond ndarray =
    unsafePerformIO
    .
    withNDArray ndarray
    $
    \ptr ->
        (
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
-- @-node:gcross.20091219130644.1370:find
-- @+node:gcross.20091219130644.1373:any/all/and/or
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
-- @-node:gcross.20091219130644.1373:any/all/and/or
-- @-node:gcross.20091219130644.1361:Folding
-- @+node:gcross.20091224210553.1569:shapeN
shape0 = ()
shape1 a = a :. () :: Int :. ()
shape2 a b = a :. b :. () :: Vec2 Int
shape3 a b c = a :. b :. c :. () :: Vec3 Int
shape4 a b c d = a :. b :. c :. d :. () :: Vec4 Int
shape5 a b c d e = a :. b :. c :. d :. e :. () :: Vec5 Int
shape6 a b c d e f = a :. b :. c :. d :. e :. f :. () :: Vec6 Int
shape7 a b c d e f g = a :. b :. c :. d :. e :. f :. g :. () :: Vec7 Int
shape8 a b c d e f g h = a :. b :. c :. d :. e :. f :. g :. h :. () :: Vec8 Int
shape9 a b c d e f g h i = a :. b :. c :. d :. e :. f :. g :. h :. i :. () :: Vec9 Int
-- @-node:gcross.20091224210553.1569:shapeN
-- @-node:gcross.20091217190104.1270:Functions
-- @-others
-- @-node:gcross.20091217190104.1264:@thin NDArray.hs
-- @-leo
