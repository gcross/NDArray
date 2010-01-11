-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1410:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1411:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
-- @-node:gcross.20091217190104.1411:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1412:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.Array.IArray hiding ((!))
import Data.Array.MArray
import Data.Array.ST
import Data.List
import qualified Data.Vec as V
import Data.Vec((:.)(..))

import Debug.Trace

import Foreign.Storable

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import System.IO.Unsafe

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Indexable
import qualified Data.NDArray.Listlike as N
import Data.NDArray.Mutable
-- @-node:gcross.20091217190104.1412:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091224104908.1441:Generators
-- @+node:gcross.20091224104908.1442:UnderTenInt
newtype UnderTenInt = UTI Int deriving (Show,Eq)
instance Arbitrary UnderTenInt where
    arbitrary = choose (1,10) >>= return.UTI
-- @-node:gcross.20091224104908.1442:UnderTenInt
-- @-node:gcross.20091224104908.1441:Generators
-- @+node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20091217190104.2176:echo
echo x = trace (show x) x
-- @-node:gcross.20091217190104.2176:echo
-- @+node:gcross.20091218141305.1337:skipList
skipList :: Int -> [a] -> [a]
skipList _ [] = []
skipList n (x:xs) = x:skipList n (drop (n-1) xs)
-- @-node:gcross.20091218141305.1337:skipList
-- @-node:gcross.20091217190104.2175:Functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091217190104.1416:<< Tests >>
    -- @+others
    -- @+node:gcross.20091217190104.1417:contiguousStridesFromShape
    [testGroup "contiguousStridesFromShape"
        -- @    @+others
        -- @+node:gcross.20091217190104.1419:length 1 case
        [testProperty "length 1 case" $
            \(x :: Int) ->
                contiguousStridesFromShape (x :. ())
                ==
                1 :. ()
        -- @-node:gcross.20091217190104.1419:length 1 case
        -- @+node:gcross.20091217190104.1422:length 2 case
        ,testProperty "length 2 case" $
            \(x :: Int) (y :: Int) ->
                contiguousStridesFromShape (x :. y :. ())
                ==
                y :. 1 :. ()
        -- @-node:gcross.20091217190104.1422:length 2 case
        -- @+node:gcross.20091217190104.1424:length 3 case
        ,testProperty "length 3 case" $
            \(x :: Int) (y :: Int) (z :: Int) ->
                contiguousStridesFromShape (x :. y :. z :. ())
                ==
                (y*z) :. z :. 1 :. ()
        -- @-node:gcross.20091217190104.1424:length 3 case
        -- @+node:gcross.20091217190104.1426:length 4 case
        ,testProperty "length 4 case" $
            \(x :: Int) (y :: Int) (z :: Int) (w :: Int) ->
                contiguousStridesFromShape (x :. y :. z :. w :. ())
                ==
                (y*w*z) :. (z*w) :. w :. 1 :. ()
        -- @-node:gcross.20091217190104.1426:length 4 case
        -- @-others
        ]
    -- @-node:gcross.20091217190104.1417:contiguousStridesFromShape
    -- @+node:gcross.20091224210553.1534:cuts
    ,testGroup "cuts"
        -- @    @+others
        -- @+node:gcross.20091224210553.1552:cutOffset
        [testGroup "cutOffset"
            -- @    @+others
            -- @+node:gcross.20091224210553.1553:()
            [testCase "()" $
                assertEqual
                    "Is the offset of a non-cut zero?"
                    0
                    (cutOffset () ())
            -- @-node:gcross.20091224210553.1553:()
            -- @+node:gcross.20091224210553.1554:All :. ()
            ,testProperty "All :. ()" $
                \(x :: Int) ->
                    0
                    ==
                    cutOffset (All :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1554:All :. ()
            -- @+node:gcross.20091224210553.1555:All :. All :. ()
            ,testProperty "All :. All :. ()" $
                \y x ->
                    0
                    ==
                    cutOffset (All :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1555:All :. All :. ()
            -- @+node:gcross.20091224210553.1556:Index :. ()
            ,testProperty "Index :. ()" $
                \a x ->
                    a*x
                    ==
                    cutOffset (Index a :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1556:Index :. ()
            -- @+node:gcross.20091224210553.1557:Index :. Index :. ()
            ,testProperty "Index :. Index :. ()" $
                \b a y x ->
                    b*y+a*x
                    ==
                    cutOffset (Index b :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1557:Index :. Index :. ()
            -- @+node:gcross.20091224210553.1558:All :. Index :. ()
            ,testProperty "All :. Index :. ()" $
                \a y x ->
                    a*x
                    ==
                    cutOffset (All :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1558:All :. Index :. ()
            -- @+node:gcross.20091224210553.1559:Index :. All :. ()
            ,testProperty "Index :. All :. ()" $
                \a y x ->
                    a*y
                    ==
                    cutOffset (Index a :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1559:Index :. All :. ()
            -- @+node:gcross.20091224210553.1560:Range :. ()
            ,testProperty "Range :. ()" $
                \al ah x ->
                    al*x
                    ==
                    cutOffset (Range al ah :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1560:Range :. ()
            -- @+node:gcross.20091224210553.1561:Range :. Range :. ()
            ,testProperty "Range :. Range :. ()" $
                \al ah bl bh x y ->
                    bl*y + al*x
                    ==
                    cutOffset (Range bl bh :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1561:Range :. Range :. ()
            -- @+node:gcross.20091224210553.1562:All :. Range :. ()
            ,testProperty "All :. Range :. ()" $
                \al ah x y ->
                    al*x
                    ==
                    cutOffset (All :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1562:All :. Range :. ()
            -- @+node:gcross.20091224210553.1563:Range :. All :. ()
            ,testProperty "Range :. All :. ()" $
                \al ah x y ->
                    al*y
                    ==
                    cutOffset (Range al ah :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1563:Range :. All :. ()
            -- @+node:gcross.20091224210553.1564:Range :. Index :. ()
            ,testProperty "Range :. All :. ()" $
                \al ah b x y ->
                    al*y + b*x
                    ==
                    cutOffset (Range al ah :. Index b :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1564:Range :. Index :. ()
            -- @+node:gcross.20091224210553.1565:Index :. Range :. ()
            ,testProperty "Index :. Range :. ()" $
                \bl bh a x y ->
                    a*y + bl*x
                    ==
                    cutOffset (Index a :. Range bl bh :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1565:Index :. Range :. ()
            -- @+node:gcross.20091224210553.1566:StridedRange :. ()
            ,testProperty "StridedRange :. ()" $
                \bl bh bs x ->
                    bl*x
                    ==
                    cutOffset (StridedRange bl bh bs :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1566:StridedRange :. ()
            -- @+node:gcross.20091224210553.1567:StridedRange :. StridedRange :. ()
            ,testProperty "StridedRange :. StridedRange :. ()" $
                \cl ch cs bl bh bs y x ->
                    bl*y + cl*x
                    ==
                    cutOffset (StridedRange bl bh bs :. StridedRange cl ch cs :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1567:StridedRange :. StridedRange :. ()
            -- @+node:gcross.20091224210553.1568:Index :. Range :. StridedRange :. ()
            ,testProperty "Index :. Range :. StridedRange :. ()" $
                \cl ch cs bl bh a x y z ->
                    a*x + bl*y + cl*z
                    ==
                    cutOffset (Index a :. Range bl bh :. StridedRange cl ch cs :. ()) (shape3 x y z)
            -- @-node:gcross.20091224210553.1568:Index :. Range :. StridedRange :. ()
            -- @-others
            ]
        -- @-node:gcross.20091224210553.1552:cutOffset
        -- @+node:gcross.20091224210553.1587:cutStrides
        ,testGroup "cutStrides"
            -- @    @+others
            -- @+node:gcross.20091224210553.1588:()
            [testCase "()" $
                assertEqual
                    "Is the offset of a non-cut zero?"
                    shape0
                    (cutStrides () shape0)
            -- @-node:gcross.20091224210553.1588:()
            -- @+node:gcross.20091224210553.1589:All :. ()
            ,testProperty "All :. ()" $
                \(x :: Int) ->
                    shape1 x
                    ==
                    cutStrides (All :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1589:All :. ()
            -- @+node:gcross.20091224210553.1590:All :. All :. ()
            ,testProperty "All :. All :. ()" $
                \y x ->
                    shape2 y x
                    ==
                    cutStrides (All :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1590:All :. All :. ()
            -- @+node:gcross.20091224210553.1591:Index :. ()
            ,testProperty "Index :. ()" $
                \a x ->
                    shape0
                    ==
                    cutStrides (Index a :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1591:Index :. ()
            -- @+node:gcross.20091224210553.1592:Index :. Index :. ()
            ,testProperty "Index :. Index :. ()" $
                \b a y x ->
                    shape0
                    ==
                    cutStrides (Index b :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1592:Index :. Index :. ()
            -- @+node:gcross.20091224210553.1593:All :. Index :. ()
            ,testProperty "All :. Index :. ()" $
                \a y x ->
                    shape1 y
                    ==
                    cutStrides (All :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1593:All :. Index :. ()
            -- @+node:gcross.20091224210553.1594:Index :. All :. ()
            ,testProperty "Index :. All :. ()" $
                \a y x ->
                    shape1 x
                    ==
                    cutStrides (Index a :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1594:Index :. All :. ()
            -- @+node:gcross.20091224210553.1595:Range :. ()
            ,testProperty "Range :. ()" $
                \al ah x ->
                    shape1 x
                    ==
                    cutStrides (Range al ah :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1595:Range :. ()
            -- @+node:gcross.20091224210553.1596:Range :. Range :. ()
            ,testProperty "Range :. Range :. ()" $
                \al ah bl bh x y ->
                    shape2 y x
                    ==
                    cutStrides (Range bl bh :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1596:Range :. Range :. ()
            -- @+node:gcross.20091224210553.1597:All :. Range :. ()
            ,testProperty "All :. Range :. ()" $
                \al ah x y ->
                    shape2 y x
                    ==
                    cutStrides (All :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1597:All :. Range :. ()
            -- @+node:gcross.20091224210553.1598:Range :. All :. ()
            ,testProperty "Range :. All :. ()" $
                \al ah x y ->
                    shape2 y x
                    ==
                    cutStrides (Range al ah :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1598:Range :. All :. ()
            -- @+node:gcross.20091224210553.1599:Range :. Index :. ()
            ,testProperty "Range :. All :. ()" $
                \al ah b x y ->
                    shape1 y
                    ==
                    cutStrides (Range al ah :. Index b :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1599:Range :. Index :. ()
            -- @+node:gcross.20091224210553.1600:Index :. Range :. ()
            ,testProperty "Index :. Range :. ()" $
                \bl bh a x y ->
                    shape1 x
                    ==
                    cutStrides (Index a :. Range bl bh :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1600:Index :. Range :. ()
            -- @+node:gcross.20091224210553.1601:StridedRange :. ()
            ,testProperty "StridedRange :. ()" $
                \bl bh bs x ->
                    shape1 (bs*x)
                    ==
                    cutStrides (StridedRange bl bh bs :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1601:StridedRange :. ()
            -- @+node:gcross.20091224210553.1602:StridedRange :. StridedRange :. ()
            ,testProperty "StridedRange :. StridedRange :. ()" $
                \cl ch cs bl bh bs y x ->
                    shape2 (bs*y) (cs*x)
                    ==
                    cutStrides (StridedRange bl bh bs :. StridedRange cl ch cs :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1602:StridedRange :. StridedRange :. ()
            -- @+node:gcross.20091224210553.1603:Index :. Range :. StridedRange :. ()
            ,testProperty "Index :. Range :. StridedRange :. ()" $
                \cl ch cs bl bh a x y z ->
                    shape2 y (cs*z)
                    ==
                    cutStrides (Index a :. Range bl bh :. StridedRange cl ch cs :. ()) (shape3 x y z)
            -- @-node:gcross.20091224210553.1603:Index :. Range :. StridedRange :. ()
            -- @-others
            ]
        -- @-node:gcross.20091224210553.1587:cutStrides
        -- @+node:gcross.20091224210553.1623:cutShape
        ,testGroup "cutShape"
            -- @    @+others
            -- @+node:gcross.20091224210553.1624:()
            [testCase "()" $
                assertEqual
                    "Is the offset of a non-cut zero?"
                    shape0
                    (cutShape () shape0)
            -- @-node:gcross.20091224210553.1624:()
            -- @+node:gcross.20091224210553.1625:All :. ()
            ,testProperty "All :. ()" $
                \(Positive x) ->
                    shape1 x
                    ==
                    cutShape (All :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1625:All :. ()
            -- @+node:gcross.20091224210553.1626:All :. All :. ()
            ,testProperty "All :. All :. ()" $
                \(Positive y) (Positive x) ->
                    shape2 y x
                    ==
                    cutShape (All :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1626:All :. All :. ()
            -- @+node:gcross.20091224210553.1627:Index :. ()
            ,testProperty "Index :. ()" $
                \(Positive x) ->
                  choose (0,x-1) >>= \a -> return $
                    shape0
                    ==
                    cutShape (Index a :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1627:Index :. ()
            -- @+node:gcross.20091224210553.1628:Index :. Index :. ()
            ,testProperty "Index :. Index :. ()" $
                \(Positive y) (Positive x) -> do
                  b <- choose (0,y-1)
                  a <- choose (0,x-1)
                  return $
                    shape0
                    ==
                    cutShape (Index b :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1628:Index :. Index :. ()
            -- @+node:gcross.20091224210553.1629:All :. Index :. ()
            ,testProperty "All :. Index :. ()" $
                \(Positive y) (Positive x) ->
                  choose (0,x-1) >>= \a -> return $
                    shape1 y
                    ==
                    cutShape (All :. Index a :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1629:All :. Index :. ()
            -- @+node:gcross.20091224210553.1630:Index :. All :. ()
            ,testProperty "Index :. All :. ()" $
                \(Positive y) (Positive x) ->
                  choose (0,y-1) >>= \a -> return $
                    shape1 x
                    ==
                    cutShape (Index a :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1630:Index :. All :. ()
            -- @+node:gcross.20091224210553.1631:Range :. ()
            ,testProperty "Range :. ()" $
                \(Positive x) -> do
                  al <- choose (0,x-1)
                  ah <- choose (al,x)
                  return $
                    shape1 (ah-al)
                    ==
                    cutShape (Range al ah :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1631:Range :. ()
            -- @+node:gcross.20091224210553.1632:Range :. Range :. ()
            ,testProperty "Range :. Range :. ()" $
                \(Positive x) (Positive y) -> do
                  al <- choose (0,x-1)
                  ah <- choose (al,x)
                  bl <- choose (0,y-1)
                  bh <- choose (bl,y)
                  return $
                    shape2 (bh-bl) (ah-al)
                    ==
                    cutShape (Range bl bh :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1632:Range :. Range :. ()
            -- @+node:gcross.20091224210553.1633:All :. Range :. ()
            ,testProperty "All :. Range :. ()" $
                \(Positive x) (Positive y) -> do
                  al <- choose (0,x-1)
                  ah <- choose (al,x)
                  return $
                    shape2 y (ah-al)
                    ==
                    cutShape (All :. Range al ah :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1633:All :. Range :. ()
            -- @+node:gcross.20091224210553.1634:Range :. All :. ()
            ,testProperty "Range :. All :. ()" $
                \(Positive x) (Positive y) -> do
                  al <- choose (0,y-1)
                  ah <- choose (al,y)
                  return $
                    shape2 (ah-al) x
                    ==
                    cutShape (Range al ah :. All :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1634:Range :. All :. ()
            -- @+node:gcross.20091224210553.1635:Range :. Index :. ()
            ,testProperty "Range :. All :. ()" $
                \(Positive x) (Positive y) -> do
                  al <- choose (0,y-1)
                  ah <- choose (al,y)
                  b <- choose (0,x-1)
                  return $
                    shape1 (ah-al)
                    ==
                    cutShape (Range al ah :. Index b :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1635:Range :. Index :. ()
            -- @+node:gcross.20091224210553.1636:Index :. Range :. ()
            ,testProperty "Index :. Range :. ()" $
                \(Positive x) (Positive y) -> do
                  a <- choose (0,y-1)
                  bl <- choose (0,x-1)
                  bh <- choose (bl,x)
                  return $
                    shape1 (bh-bl)
                    ==
                    cutShape (Index a :. Range bl bh :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1636:Index :. Range :. ()
            -- @+node:gcross.20091224210553.1637:StridedRange :. ()
            ,testProperty "StridedRange :. ()" $
                \(Positive xm1) -> do
                  let x = xm1+1
                  bl <- choose (0,x-1)
                  bh <- choose (bl+1,x)
                  bs <- choose (1,bh-bl)
                  return $
                    shape1 ((bh-bl-1) `div` bs + 1)
                    ==
                    cutShape (StridedRange bl bh bs :. ()) (shape1 x)
            -- @-node:gcross.20091224210553.1637:StridedRange :. ()
            -- @+node:gcross.20091224210553.1638:StridedRange :. StridedRange :. ()
            ,testProperty "StridedRange :. StridedRange :. ()" $
                \(Positive xm1) (Positive ym1) -> do
                  let x = xm1+1
                      y = ym1+1
                  bl <- choose (0,y-1)
                  bh <- choose (bl+1,y)
                  bs <- choose (1,bh-bl)
                  cl <- choose (0,x-1)
                  ch <- choose (cl+1,x)
                  cs <- choose (1,ch-cl)
                  return $
                    shape2 ((bh-bl-1) `div` bs + 1) ((ch-cl-1) `div` cs + 1)
                    ==
                    cutShape (StridedRange bl bh bs :. StridedRange cl ch cs :. ()) (shape2 y x)
            -- @-node:gcross.20091224210553.1638:StridedRange :. StridedRange :. ()
            -- @+node:gcross.20091224210553.1639:Index :. Range :. StridedRange :. ()
            ,testProperty "Index :. Range :. StridedRange :. ()" $
                \(Positive x) (Positive y) (Positive z) -> do
                  a <- choose (0,x-1)
                  bl <- choose (0,y-1)
                  bh <- choose (bl,y)
                  cl <- choose (0,z-1)
                  ch <- choose (cl+1,z)
                  cs <- choose (1,ch-cl)
                  return $
                    shape2 (bh-bl) ((ch-cl-1) `div` cs + 1)
                    ==
                    cutShape (Index a :. Range bl bh :. StridedRange cl ch cs :. ()) (shape3 x y z)
            -- @-node:gcross.20091224210553.1639:Index :. Range :. StridedRange :. ()
            -- @-others
            ]
        -- @-node:gcross.20091224210553.1623:cutShape
        -- @+node:gcross.20091225065853.1574:cut
        ,testGroup "cut" $
            let makeCutTests :: (forall indexType . forall dataType . (Indexable indexType, Storable dataType) => NDArray indexType dataType -> [dataType]) -> [Test.Framework.Test]
                makeCutTests toList =
                    -- @            @+others
                    -- @+node:gcross.20091225065853.1575:1D, null cut
                    [testProperty "1D, null cut" $
                        liftA2 (==)
                            (toList . cut (All :. ()) . fromList)
                            (id :: [Int] -> [Int])
                    -- @-node:gcross.20091225065853.1575:1D, null cut
                    -- @+node:gcross.20091225065853.1576:1D, skip 1
                    ,testProperty "1D, skip 1" $
                        \(lst :: [Int]) ->
                            liftA2 (==)
                                (toList . cut (StridedRange 0 (length lst) 1 :. ()) . fromList)
                                (id :: [Int] -> [Int])
                            lst
                    -- @-node:gcross.20091225065853.1576:1D, skip 1
                    -- @+node:gcross.20091225065853.1577:1D, skip 2
                    ,testProperty "1D, skip 2" $
                        \(lst :: [Int]) ->
                            liftA2 (==)
                                (toList . cut (StridedRange 0 (length lst) 2 :. ()) . fromList)
                                (skipList 2)
                            lst
                    -- @-node:gcross.20091225065853.1577:1D, skip 2
                    -- @+node:gcross.20091225065853.1578:1D, skip 3
                    ,testProperty "1D, skip 3" $
                        \(lst :: [Int]) ->
                            liftA2 (==)
                                (toList . cut (StridedRange 0 (length lst) 3 :. ()) . fromList)
                                (skipList 3)
                            lst
                    -- @-node:gcross.20091225065853.1578:1D, skip 3
                    -- @+node:gcross.20091225065853.1579:1D, skip N
                    ,testProperty "1D, skip N" $
                        \(lst :: [Int]) (Positive n) ->
                            liftA2 (==)
                                (toList . cut (StridedRange 0 (length lst) n :. ()) . fromList)
                                (skipList n)
                            lst
                    -- @-node:gcross.20091225065853.1579:1D, skip N
                    -- @+node:gcross.20091225065853.1580:1D, arbitrary (lo,hi,skip)
                    ,testProperty "1D, arbitrary (lo,hi,skip)" $ mapSize (\n -> if n > 10 then 10 else n) $
                        \
                        (Positive x)
                        (Positive y)
                        (Positive z)
                        (Positive w)
                        ->
                        let a = x
                            b = x+y
                            c = x+y+z
                        in (toList . cut (StridedRange a c (b-a) :. ()) . fromList) [0..c+w] == [a,b..c-1]
                    -- @-node:gcross.20091225065853.1580:1D, arbitrary (lo,hi,skip)
                    -- @+node:gcross.20091225065853.1581:1D, sum arbitrary (lo,hi,skip)
                    ,testProperty "1D, sum over arbitrary (lo,hi,skip)" $ mapSize (\n -> if n > 10 then 10 else n) $
                        \
                        (Positive x)
                        (Positive y)
                        (Positive z)
                        (Positive w)
                        ->
                        let a = x
                            b = x+y
                            c = x+y+z
                        in (N.sum . cut (StridedRange a c (b-a) :. ()) . fromList) [0..c+w] == sum [a,b..c-1]
                    -- @-node:gcross.20091225065853.1581:1D, sum arbitrary (lo,hi,skip)
                    -- @+node:gcross.20091225065853.1582:2D, 3x3, skip 2
                    ,testCase "2D, 3x3, skip 2" $
                        assertEqual
                            "Is the extracted list correct?"
                            [1,3,7,9::Int]
                            .
                            toList
                            .
                            cut (StridedRange 0 3 2 :. StridedRange 0 3 2 :. ())
                            .
                            fromListWithShape (shape2 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9
                            ]
                    -- @-node:gcross.20091225065853.1582:2D, 3x3, skip 2
                    -- @+node:gcross.20091225065853.1583:2D, 3x3, StridedRange 1 3 2 :. StridedRange 1 3 2 :. ()
                    ,testCase "2D, 3x3, StridedRange 1 3 2 :. StridedRange 1 3 2 :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [5::Int]
                            .
                            toList
                            .
                            cut (StridedRange 1 3 2 :. StridedRange 1 3 2 :. ())
                            .
                            fromListWithShape (shape2 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9
                            ]
                    -- @-node:gcross.20091225065853.1583:2D, 3x3, StridedRange 1 3 2 :. StridedRange 1 3 2 :. ()
                    -- @+node:gcross.20091225065853.1584:2D, 3x3, Index 1 :. All :. ()
                    ,testCase "2D, 3x3, Index 1 :. All :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [4,5,6::Int]
                            .
                            toList
                            .
                            cut (Index 1 :. All :. ())
                            .
                            fromListWithShape (shape2 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9
                            ]
                    -- @-node:gcross.20091225065853.1584:2D, 3x3, Index 1 :. All :. ()
                    -- @+node:gcross.20091225065853.1585:2D, 3x3, All :. Index 1 :. ()
                    ,testCase "2D, 3x3, All :. Index 1 :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [2,5,8::Int]
                            .
                            toList
                            .
                            cut (All :. Index 1 :. ())
                            .
                            fromListWithShape (shape2 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9
                            ]
                    -- @-node:gcross.20091225065853.1585:2D, 3x3, All :. Index 1 :. ()
                    -- @+node:gcross.20091225065853.1586:3D, 3x3x3, All :. All:. Index 0 :. ()
                    ,testCase "3D, 3x3x3, All :. All:. Index 0 :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [1,4,7
                            ,9,6,3
                            ,1,2,3::Int
                            ]
                            .
                            toList
                            .
                            cut (All :. All :. Index 0 :. ())
                            .
                            fromListWithShape (shape3 3 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9

                            ,9,8,7
                            ,6,5,4
                            ,3,2,1

                            ,1,5,9
                            ,2,6,7
                            ,3,4,8
                            ]
                    -- @-node:gcross.20091225065853.1586:3D, 3x3x3, All :. All:. Index 0 :. ()
                    -- @+node:gcross.20091225065853.1587:3D, 3x3x3, All :. All:. Index 1 :. ()
                    ,testCase "3D, 3x3x3, All :. All:. Index 1 :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [2,5,8
                            ,8,5,2
                            ,5,6,4::Int
                            ]
                            .
                            toList
                            .
                            cut (All :. All :. Index 1 :. ())
                            .
                            fromListWithShape (shape3 3 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9

                            ,9,8,7
                            ,6,5,4
                            ,3,2,1

                            ,1,5,9
                            ,2,6,7
                            ,3,4,8
                            ]
                    -- @-node:gcross.20091225065853.1587:3D, 3x3x3, All :. All:. Index 1 :. ()
                    -- @+node:gcross.20091225065853.1588:3D, 3x3x3, All :. All:. Index 2 :. ()
                    ,testCase "3D, 3x3x3, All :. All:. Index 2 :. ()" $
                        assertEqual
                            "Is the extracted list correct?"
                            [3,6,9
                            ,7,4,1
                            ,9,7,8::Int
                            ]
                            .
                            toList
                            .
                            cut (All :. All :. Index 2 :. ())
                            .
                            fromListWithShape (shape3 3 3 3)
                            $
                            [1,2,3
                            ,4,5,6
                            ,7,8,9

                            ,9,8,7
                            ,6,5,4
                            ,3,2,1

                            ,1,5,9
                            ,2,6,7
                            ,3,4,8
                            ]
                    -- @-node:gcross.20091225065853.1588:3D, 3x3x3, All :. All:. Index 2 :. ()
                    -- @+node:gcross.20091225065853.1589:3D, MxNxO, All :. All:. Index i :. ()
                    ,testProperty "3D, MxNxO, All :. All:. Index i :. ()" $ \(UTI m) (UTI n) (UTI o) ->
                        choose (0,o-1) >>= \i ->
                        vectorOf (m*n*o) (arbitrary :: Gen Int) >>=
                            return . liftA2 (==)
                                (skipList o . drop i)
                                (toList . cut (All :. All :. Index i :. ()) . fromListWithShape (shape3 m n o))
                    -- @-node:gcross.20091225065853.1589:3D, MxNxO, All :. All:. Index i :. ()
                    -- @-others
                    ]
            in [testGroup "toList" $ makeCutTests toList
               ,testGroup "reverse . foldl (flip (:)) []" $ makeCutTests (reverse . N.foldl (flip (:)) [])
               ]
        -- @-node:gcross.20091225065853.1574:cut
        -- @-others
        ]
    -- @-node:gcross.20091224210553.1534:cuts
    -- @+node:gcross.20091224210553.1645:folding
    ,testGroup "folding"
        -- @    @+others
        -- @+node:gcross.20091224210553.1646:sum
        [testProperty "sum" $
            liftA2 (==) (N.sum . fromList) (sum :: [Int] -> Int)
        -- @-node:gcross.20091224210553.1646:sum
        -- @+node:gcross.20091224210553.1647:product
        ,testProperty "product" $
            liftA2 (==) (N.product . fromList) (product :: [Int] -> Int)
        -- @-node:gcross.20091224210553.1647:product
        -- @+node:gcross.20091224210553.1648:and
        ,testProperty "and" $
            \flag ->
                liftA2 (==) (N.and . fromList) (and :: [Bool] -> Bool) .
                    if flag then map (const True) else id
        -- @-node:gcross.20091224210553.1648:and
        -- @+node:gcross.20091224210553.1649:or
        ,testProperty "or" $
            \flag ->
                liftA2 (==) (N.or . fromList) (or :: [Bool] -> Bool) .
                    if flag then map (const False) else id
        -- @-node:gcross.20091224210553.1649:or
        -- @-others
        ]
    -- @-node:gcross.20091224210553.1645:folding
    -- @+node:gcross.20091224210553.1651:fromList/toList
    ,testProperty "fromList/toList" $
        liftA2 (==) (toList . fromList :: [Int] -> [Int]) id
    -- @-node:gcross.20091224210553.1651:fromList/toList
    -- @+node:gcross.20091226102316.1372:(!)
    ,testGroup "(!)"
        -- @    @+others
        -- @+node:gcross.20091226102316.1373:1D
        [testProperty "1D" $
            \(list :: [Int]) -> (not.null) list ==>
                choose (0,length list-1) >>= \index -> return $
                    liftA2 (==)
                        ((! i1 index) . fromList)
                        (!! index)
                        list
        -- @-node:gcross.20091226102316.1373:1D
        -- @+node:gcross.20091226102316.1375:2D
        ,testProperty "2D" $ do
            (UTI m) <- arbitrary
            (UTI n) <- arbitrary
            list <- vectorOf (m*n) (arbitrary :: Gen Int)
            i <- choose (0,m-1)
            j <- choose (0,n-1)
            let shape = shape2 m n
            let index = i2 i j
            let offset = i*n+j
            return $
                liftA2 (==)
                    ((! index) . fromListWithShape shape)
                    (!! offset)
                    list
        -- @-node:gcross.20091226102316.1375:2D
        -- @+node:gcross.20091226102316.1377:3D
        ,testProperty "3D" $ do
            (UTI m) <- arbitrary
            (UTI n) <- arbitrary
            (UTI o) <- arbitrary
            list <- vectorOf (m*n*o) (arbitrary :: Gen Int)
            i <- choose (0,m-1)
            j <- choose (0,n-1)
            k <- choose (0,o-1)
            let shape = shape3 m n o
            let index = i3 i j k
            let offset = i*n*o + j*o+k
            return $
                liftA2 (==)
                    ((! index) . fromListWithShape shape)
                    (!! offset)
                    list
        -- @-node:gcross.20091226102316.1377:3D
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20091226102316.1372:(!)
    -- @+node:gcross.20100110123138.1770:Mutable
    ,testGroup "Mutable"
        -- @    @+others
        -- @+node:gcross.20100110123138.1772:createFromList / readIntoList
        [testProperty "createFromList/readIntoList" $
            liftA2 (==) (unsafePerformIO . (createFromList >=> readIntoList) :: [Int] -> [Int]) id
        -- @-node:gcross.20100110123138.1772:createFromList / readIntoList
        -- @+node:gcross.20100110123138.1790:readMutableNDArray
        ,testGroup "readMutableNDArray"
            -- @    @+others
            -- @+node:gcross.20100110123138.1791:1D
            [testProperty "1D" $
                \(list :: [Int]) -> (not.null) list ==>
                    choose (0,length list-1) >>= \index -> return $
                        liftA2 (==)
                            (unsafePerformIO . (createFromList >=> (`readMutableNDArray` i1 index)))
                            (!! index)
                            list
            -- @-node:gcross.20100110123138.1791:1D
            -- @+node:gcross.20100110123138.1792:2D
            ,testProperty "2D" $ do
                (UTI m) <- arbitrary
                (UTI n) <- arbitrary
                list <- vectorOf (m*n) (arbitrary :: Gen Int)
                i <- choose (0,m-1)
                j <- choose (0,n-1)
                let shape = shape2 m n
                let index = i2 i j
                let offset = i*n+j
                return $
                    liftA2 (==)
                        (unsafePerformIO . (createFromListWithShape shape >=> (`readMutableNDArray` index)))
                        (!! offset)
                        list
            -- @-node:gcross.20100110123138.1792:2D
            -- @+node:gcross.20100110123138.1793:3D
            ,testProperty "3D" $ do
                (UTI m) <- arbitrary
                (UTI n) <- arbitrary
                (UTI o) <- arbitrary
                list <- vectorOf (m*n*o) (arbitrary :: Gen Int)
                i <- choose (0,m-1)
                j <- choose (0,n-1)
                k <- choose (0,o-1)
                let shape = shape3 m n o
                let index = i3 i j k
                let offset = i*n*o + j*o+k
                return $
                    liftA2 (==)
                        (unsafePerformIO . (createFromListWithShape shape >=> (`readMutableNDArray` index)))
                        (!! offset)
                        list
            -- @-node:gcross.20100110123138.1793:3D
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20100110123138.1790:readMutableNDArray
        -- @+node:gcross.20100110123138.1798:writeMutableNDArray
        ,testGroup "writeMutableNDArray"
            -- @    @+others
            -- @+node:gcross.20100110123138.1807:1D
            [testProperty "1D" $ do
                size <- choose (1,10)
                number_of_writes <- choose (1,size*size)
                write_locations :: [Int] <- vectorOf number_of_writes (choose (0,size-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromList (replicate size 0)
                        forM_ (zip write_locations write_values) $
                            \(location,value) ->
                                writeMutableNDArray ndarray (i1 location) value
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray (0,size-1) 0
                        forM_ (zip write_locations write_values) $
                            \(location,value) ->
                                writeArray array location value
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1807:1D
            -- @+node:gcross.20100110123138.1799:2D
            ,testProperty "2D" $ do
                size1 <- choose (1,10)
                size2 <- choose (1,10)
                number_of_writes <- choose (1,size1*size2)
                write_locations_1 :: [Int] <- vectorOf number_of_writes (choose (0,size1-1))
                write_locations_2 :: [Int] <- vectorOf number_of_writes (choose (0,size2-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromListWithShape (shape2 size1 size2) (replicate (size1*size2) 0)
                        forM_ (zip3 write_locations_1 write_locations_2 write_values) $
                            \(location1,location2,value) ->
                                writeMutableNDArray ndarray (i2 location1 location2) value
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray ((0,0),(size1-1,size2-1)) 0
                        forM_ (zip3 write_locations_1 write_locations_2 write_values) $
                            \(location1,location2,value) ->
                                writeArray array (location1,location2) value
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1799:2D
            -- @+node:gcross.20100110123138.1811:3D
            ,testProperty "3D" $ do
                size1 <- choose (1,10)
                size2 <- choose (1,10)
                size3 <- choose (1,10)
                number_of_writes <- choose (1,size1*size2*size3)
                write_locations_1 :: [Int] <- vectorOf number_of_writes (choose (0,size1-1))
                write_locations_2 :: [Int] <- vectorOf number_of_writes (choose (0,size2-1))
                write_locations_3 :: [Int] <- vectorOf number_of_writes (choose (0,size3-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromListWithShape (shape3 size1 size2 size3) (replicate (size1*size2*size3) 0)
                        forM_ (zip4 write_locations_1 write_locations_2 write_locations_3 write_values) $
                            \(location1,location2,location3,value) ->
                                writeMutableNDArray ndarray (i3 location1 location2 location3) value
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray ((0,0,0),(size1-1,size2-1,size3-1)) 0
                        forM_ (zip4 write_locations_1 write_locations_2 write_locations_3 write_values) $
                            \(location1,location2,location3,value) ->
                                writeArray array (location1,location2,location3) value
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1811:3D
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20100110123138.1798:writeMutableNDArray
        -- @+node:gcross.20100110123138.1804:modifyMutableNDArray
        ,testGroup "modifyMutableNDArray"
            -- @    @+others
            -- @+node:gcross.20100110123138.1805:1D
            [testProperty "1D" $ do
                size <- choose (1,10)
                number_of_writes <- choose (1,size*size)
                write_locations :: [Int] <- vectorOf number_of_writes (choose (0,size-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromList (replicate size 0)
                        forM_ (zip write_locations write_values) $
                            \(location,value) ->
                                modifyMutableNDArray ndarray (+value) (i1 location)
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray (0,size-1) 0
                        forM_ (zip write_locations write_values) $
                            \(location,value) ->
                                readArray array location
                                >>=
                                return . (+value)
                                >>=
                                writeArray array location
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1805:1D
            -- @+node:gcross.20100110123138.1809:2D
            ,testProperty "2D" $ do
                size1 <- choose (1,10)
                size2 <- choose (1,10)
                number_of_writes <- choose (1,size1*size2)
                write_locations_1 :: [Int] <- vectorOf number_of_writes (choose (0,size1-1))
                write_locations_2 :: [Int] <- vectorOf number_of_writes (choose (0,size2-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromListWithShape (shape2 size1 size2) (replicate (size1*size2) 0)
                        forM_ (zip3 write_locations_1 write_locations_2 write_values) $
                            \(location1,location2,value) ->
                                modifyMutableNDArray ndarray (+value) (i2 location1 location2)
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray ((0,0),(size1-1,size2-1)) 0
                        forM_ (zip3 write_locations_1 write_locations_2 write_values) $
                            \(location1,location2,value) ->
                                readArray array (location1,location2)
                                >>=
                                return . (+value)
                                >>=
                                writeArray array (location1,location2)
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1809:2D
            -- @+node:gcross.20100110123138.1813:3D
            ,testProperty "3D" $ do
                size1 <- choose (1,10)
                size2 <- choose (1,10)
                size3 <- choose (1,10)
                number_of_writes <- choose (1,size1*size2*size3)
                write_locations_1 :: [Int] <- vectorOf number_of_writes (choose (0,size1-1))
                write_locations_2 :: [Int] <- vectorOf number_of_writes (choose (0,size2-1))
                write_locations_3 :: [Int] <- vectorOf number_of_writes (choose (0,size3-1))
                write_values :: [Int] <- vectorOf number_of_writes arbitrary
                let ndarray_list = unsafePerformIO $ do
                        ndarray <- createFromListWithShape (shape3 size1 size2 size3) (replicate (size1*size2*size3) 0)
                        forM_ (zip4 write_locations_1 write_locations_2 write_locations_3 write_values) $
                            \(location1,location2,location3,value) ->
                                modifyMutableNDArray ndarray (+value) (i3 location1 location2 location3)
                        readIntoList ndarray
                    array_list = elems . runSTUArray $ do
                        array <- newArray ((0,0,0),(size1-1,size2-1,size3-1)) 0
                        forM_ (zip4 write_locations_1 write_locations_2 write_locations_3 write_values) $
                            \(location1,location2,location3,value) ->
                                readArray array (location1,location2,location3)
                                >>=
                                return . (+value)
                                >>=
                                writeArray array (location1,location2,location3)
                        return array
                return $ ndarray_list == array_list
            -- @-node:gcross.20100110123138.1813:3D
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20100110123138.1804:modifyMutableNDArray
        -- @+node:gcross.20100110123138.1777:folding
        ,testGroup "folding" $
            -- @    @+others
            -- @+node:gcross.20100110123138.1774:sumMutableNDArray
            [testProperty "sumMutableNDArray" $
                liftA2 (==) (unsafePerformIO . (createFromList >=> sumMutableNDArray)) (sum :: [Int] -> Int)
            -- @-node:gcross.20100110123138.1774:sumMutableNDArray
            -- @+node:gcross.20100110123138.1779:productMutableNDArray
            ,testProperty "productMutableNDArray" $
                liftA2 (==) (unsafePerformIO . (createFromList >=> productMutableNDArray)) (product :: [Int] -> Int)
            -- @-node:gcross.20100110123138.1779:productMutableNDArray
            -- @+node:gcross.20100110123138.1783:andMutableNDArray
            ,testProperty "andMutableNDArray" $
                \flag ->
                    liftA2 (==) (unsafePerformIO . (createFromList >=> andMutableNDArray)) (and :: [Bool] -> Bool) .
                        if flag then map (const True) else id
            -- @-node:gcross.20100110123138.1783:andMutableNDArray
            -- @+node:gcross.20100110123138.1785:orMutableNDArray
            ,testProperty "orMutableNDArray" $
                \flag ->
                    liftA2 (==) (unsafePerformIO . (createFromList >=> orMutableNDArray)) (or :: [Bool] -> Bool) .
                        if flag then map (const False) else id
            -- @-node:gcross.20100110123138.1785:orMutableNDArray
            -- @-others
            ]
        -- @-node:gcross.20100110123138.1777:folding
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20100110123138.1770:Mutable
    -- @-others
    -- @-node:gcross.20091217190104.1416:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091217190104.1410:@thin test.hs
-- @-leo
