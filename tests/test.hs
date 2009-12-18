-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1410:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1411:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091217190104.1411:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1412:<< Import needed modules >>
import qualified Data.Vec as V
import Data.Vec((:.)(..))

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import System.IO.Unsafe

import Data.NDArray
-- @-node:gcross.20091217190104.1412:<< Import needed modules >>
-- @nl

-- @+others
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
                x :. ()
        -- @-node:gcross.20091217190104.1419:length 1 case
        -- @+node:gcross.20091217190104.1422:length 2 case
        ,testProperty "length 2 case" $
            \(x :: Int) (y :: Int) ->
                contiguousStridesFromShape (x :. y :. ())
                ==
                (x*y) :. y :. ()
        -- @-node:gcross.20091217190104.1422:length 2 case
        -- @+node:gcross.20091217190104.1424:length 3 case
        ,testProperty "length 3 case" $
            \(x :: Int) (y :: Int) (z :: Int) ->
                contiguousStridesFromShape (x :. y :. z :. ())
                ==
                (x*y*z) :. (y*z) :. z :. ()
        -- @-node:gcross.20091217190104.1424:length 3 case
        -- @+node:gcross.20091217190104.1426:length 4 case
        ,testProperty "length 4 case" $
            \(x :: Int) (y :: Int) (z :: Int) (w :: Int) ->
                contiguousStridesFromShape (x :. y :. z :. w :. ())
                ==
                (x*y*w*z) :. (y*w*z) :. (z*w) :. w :. ()
        -- @-node:gcross.20091217190104.1426:length 4 case
        -- @-others
        ]
    -- @-node:gcross.20091217190104.1417:contiguousStridesFromShape
    -- @+node:gcross.20091217190104.1428:Cut
    ,testGroup "cuts"
        -- @    @+others
        -- @+node:gcross.20091217190104.1429:cutOffset
        [testGroup "cutOffset"
            -- @    @+others
            -- @+node:gcross.20091217190104.1431:()
            [testCase "()" $
                assertEqual
                    "Is the offset of a non-cut zero?"
                    0
                    (cutOffset () ())
            -- @-node:gcross.20091217190104.1431:()
            -- @+node:gcross.20091217190104.1433:() :. ()
            ,testProperty "() :. ()" $
                \(x :: Int) -> 0 == cutOffset (() :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1433:() :. ()
            -- @+node:gcross.20091217190104.1435:() :. () :. ()
            ,testProperty "() :. () :. ()" $
                \(y :: Int) (x :: Int) -> 0 == cutOffset (() :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1435:() :. () :. ()
            -- @+node:gcross.20091217190104.1439:a :. ()
            ,testProperty "a :. ()" $
                \(a :: Int) (x :: Int) -> a*x == cutOffset (a :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1439:a :. ()
            -- @+node:gcross.20091217190104.1441:b :. a :. ()
            ,testProperty "b :. a :. ()" $
                \(b :: Int) (a :: Int) (y :: Int) (x :: Int) -> b*y+a*x == cutOffset (b :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1441:b :. a :. ()
            -- @+node:gcross.20091217190104.1443:() :. a :. ()
            ,testProperty "() :. a :. ()" $
                \(a :: Int) (y :: Int) (x :: Int) -> a*x == cutOffset (() :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1443:() :. a :. ()
            -- @+node:gcross.20091217190104.1445:a :. () :. ()
            ,testProperty "a :. () :. ()" $
                \(a :: Int) (y :: Int) (x :: Int) -> a*y == cutOffset (a :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1445:a :. () :. ()
            -- @+node:gcross.20091217190104.1447:(al,ah) :. ()
            ,testProperty "(al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 -> al*x == cutOffset ((al,ah) :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1447:(al,ah) :. ()
            -- @+node:gcross.20091217190104.1449:(bl,bh) :. (al,ah) :. ()
            ,testProperty "(bl,bh) :. (al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> bl*y + al*x == cutOffset ((bl,bh) :. (al,ah) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1449:(bl,bh) :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1451:() :. (al,ah) :. ()
            ,testProperty "() :. (al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> al*x == cutOffset (() :. (al,ah) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1451:() :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1453:(al,ah) :. () :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> al*y == cutOffset ((al,ah) :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1453:(al,ah) :. () :. ()
            -- @+node:gcross.20091217190104.1455:(al,ah) :. b :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (b :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> al*y + b*x == cutOffset ((al,ah) :. b :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1455:(al,ah) :. b :. ()
            -- @+node:gcross.20091217190104.1457:a :. (bl,bh) :. ()
            ,testProperty "a :. (bl,bh) :. ()" $
                \(bl :: Int)
                 (bh :: Int)
                 (a :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> a*y + bl*x == cutOffset (a :. (bl,bh) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1457:a :. (bl,bh) :. ()
            -- @+node:gcross.20091217190104.1461:(bl,bh,bs) :. ()
            ,testProperty "(bl,bh,bs) :. ()" $
                \(bl :: Int)
                 (bh :: Int)
                 (bs :: Int)
                 (x :: Int)
                 -> bl*x == cutOffset ((bl,bh,bs) :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1461:(bl,bh,bs) :. ()
            -- @+node:gcross.20091217190104.1463:(bl,bh,bs) :. (cl,ch,cs) :. ()
            ,testProperty "(bl,bh,bs) :. (cl,ch,cs) :. ()" $
                \(cl :: Int)
                 (ch :: Int)
                 (cs :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (bs :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> bl*y + cl*x == cutOffset ((bl,bh,bs) :. (cl,ch,cs) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1463:(bl,bh,bs) :. (cl,ch,cs) :. ()
            -- @+node:gcross.20091217190104.1465:a :. (bl,bh) :. (cl,ch,cs) :. ()
            ,testProperty "a :. (bl,bh) :. (cl,ch,cs) :. ()" $
                \(cl :: Int)
                 (ch :: Int)
                 (cs :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (a :: Int)
                 (x :: Int)
                 (y :: Int)
                 (z :: Int)
                 -> a*x + bl*y + cl*z == cutOffset (a :. (bl,bh) :. (cl,ch,cs) :. ()) (x :. y :. z :. ())
            -- @-node:gcross.20091217190104.1465:a :. (bl,bh) :. (cl,ch,cs) :. ()
            -- @-others
            ]
        -- @-node:gcross.20091217190104.1429:cutOffset
        -- @+node:gcross.20091217190104.1483:cutStrides
        ,testGroup "cutStrides"
            -- @    @+others
            -- @+node:gcross.20091217190104.1484:()
            [testCase "()" $
                assertEqual
                    "Is non-cut the identity on ()?"
                    ()
                    (cutStrides () ())
            -- @-node:gcross.20091217190104.1484:()
            -- @+node:gcross.20091217190104.1485:() :. ()
            ,testProperty "() :. ()" $
                \(x :: Int) ->
                    x :. ()
                    ==
                    cutStrides (() :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1485:() :. ()
            -- @+node:gcross.20091217190104.1486:() :. () :. ()
            ,testProperty "() :. () :. ()" $
                \(y :: Int) (x :: Int) ->
                    y :. x :. ()
                    ==
                    cutStrides (() :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1486:() :. () :. ()
            -- @+node:gcross.20091217190104.1487:a :. ()
            ,testProperty "a :. ()" $
                \(a :: Int) (x :: Int) ->
                    ()
                    ==
                    cutStrides (a :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1487:a :. ()
            -- @+node:gcross.20091217190104.1488:b :. a :. ()
            ,testProperty "b :. a :. ()" $
                \(b :: Int) (a :: Int) (y :: Int) (x :: Int) ->
                    ()
                    ==
                    cutStrides (b :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1488:b :. a :. ()
            -- @+node:gcross.20091217190104.1489:() :. a :. ()
            ,testProperty "() :. a :. ()" $
                \(a :: Int) (y :: Int) (x :: Int) ->
                    y :. ()
                    ==
                    cutStrides (() :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1489:() :. a :. ()
            -- @+node:gcross.20091217190104.1490:a :. () :. ()
            ,testProperty "a :. () :. ()" $
                \(a :: Int) (y :: Int) (x :: Int) ->
                    x :. ()
                    ==
                    cutStrides (a :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1490:a :. () :. ()
            -- @+node:gcross.20091217190104.1491:(al,ah) :. ()
            ,testProperty "(al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 -> x :. () == cutStrides ((al,ah) :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1491:(al,ah) :. ()
            -- @+node:gcross.20091217190104.1492:(bl,bh) :. (al,ah) :. ()
            ,testProperty "(bl,bh) :. (al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> y :. x :. () == cutStrides ((bl,bh) :. (al,ah) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1492:(bl,bh) :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1493:() :. (al,ah) :. ()
            ,testProperty "() :. (al,ah) :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> y :. x :. () == cutStrides (() :. (al,ah) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1493:() :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1494:(al,ah) :. () :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> y :. x :. () == cutStrides ((al,ah) :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1494:(al,ah) :. () :. ()
            -- @+node:gcross.20091217190104.1495:(al,ah) :. b :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \(al :: Int)
                 (ah :: Int)
                 (b :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> y :. () == cutStrides ((al,ah) :. b :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1495:(al,ah) :. b :. ()
            -- @+node:gcross.20091217190104.1496:a :. (bl,bh) :. ()
            ,testProperty "a :. (bl,bh) :. ()" $
                \(bl :: Int)
                 (bh :: Int)
                 (a :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> x :. () == cutStrides (a :. (bl,bh) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1496:a :. (bl,bh) :. ()
            -- @+node:gcross.20091217190104.1497:(bl,bh,bs) :. ()
            ,testProperty "(bl,bh,bs) :. ()" $
                \(bl :: Int)
                 (bh :: Int)
                 (bs :: Int)
                 (x :: Int)
                 -> (bs*x) :. () == cutStrides ((bl,bh,bs) :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1497:(bl,bh,bs) :. ()
            -- @+node:gcross.20091217190104.1498:(bl,bh,bs) :. (cl,ch,cs) :. ()
            ,testProperty "(bl,bh,bs) :. (cl,ch,cs) :. ()" $
                \(cl :: Int)
                 (ch :: Int)
                 (cs :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (bs :: Int)
                 (x :: Int)
                 (y :: Int)
                 -> (bs*y) :. (cs*x) :. () == cutStrides ((bl,bh,bs) :. (cl,ch,cs) :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1498:(bl,bh,bs) :. (cl,ch,cs) :. ()
            -- @+node:gcross.20091217190104.1499:a :. (bl,bh) :. (cl,ch,cs) :. ()
            ,testProperty "a :. (bl,bh) :. (cl,ch,cs) :. ()" $
                \(cl :: Int)
                 (ch :: Int)
                 (cs :: Int)
                 (bl :: Int)
                 (bh :: Int)
                 (a :: Int)
                 (x :: Int)
                 (y :: Int)
                 (z :: Int)
                 -> y :. (cs*z) :. () == cutStrides (a :. (bl,bh) :. (cl,ch,cs) :. ()) (x :. y :. z :. ())
            -- @-node:gcross.20091217190104.1499:a :. (bl,bh) :. (cl,ch,cs) :. ()
            -- @-others
            ]
        -- @-node:gcross.20091217190104.1483:cutStrides
        -- @+node:gcross.20091217190104.1519:cutBounds
        ,testGroup "cutBounds"
            -- @    @+others
            -- @+node:gcross.20091217190104.1520:()
            [testCase "()" $
                assertEqual
                    "Is non-cut the identity on ()?"
                    ()
                    (cutBounds () ())
            -- @-node:gcross.20091217190104.1520:()
            -- @+node:gcross.20091217190104.1521:() :. ()
            ,testProperty "() :. ()" $
                \(x :: Int) ->
                    x :. ()
                    ==
                    cutBounds (() :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1521:() :. ()
            -- @+node:gcross.20091217190104.1522:() :. () :. ()
            ,testProperty "() :. () :. ()" $
                \(y :: Int) (x :: Int) ->
                    y :. x :. ()
                    ==
                    cutBounds (() :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1522:() :. () :. ()
            -- @+node:gcross.20091217190104.1523:a :. ()
            ,testProperty "a :. ()" $
                \
                (Positive (a :: Int))
                (Positive (x :: Int))
                ->
                    ()
                    ==
                    cutBounds (a :. ()) (x :. ())
            -- @-node:gcross.20091217190104.1523:a :. ()
            -- @+node:gcross.20091217190104.1524:b :. a :. ()
            ,testProperty "b :. a :. ()" $
                \
                (Positive (b :: Int))
                (Positive (a :: Int))
                (Positive (y :: Int))
                (Positive (x :: Int))
                ->
                    ()
                    ==
                    cutBounds (b :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1524:b :. a :. ()
            -- @+node:gcross.20091217190104.1525:() :. a :. ()
            ,testProperty "() :. a :. ()" $
                \
                (Positive (a :: Int))
                (Positive (y :: Int))
                (Positive (x :: Int))
                ->
                    y :. ()
                    ==
                    cutBounds (() :. a :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1525:() :. a :. ()
            -- @+node:gcross.20091217190104.1526:a :. () :. ()
            ,testProperty "a :. () :. ()" $
                \
                (Positive (a :: Int))
                (Positive (y :: Int))
                (Positive (x :: Int))
                ->
                    x :. ()
                    ==
                    cutBounds (a :. () :. ()) (y :. x :. ())
            -- @-node:gcross.20091217190104.1526:a :. () :. ()
            -- @+node:gcross.20091217190104.1527:(al,ah) :. ()
            ,testProperty "(al,ah) :. ()" $
                \
                (Positive (a1 :: Int))
                (Positive (a2 :: Int))
                (Positive (x :: Int))
                 -> let ah = a1 `max` a2
                        al = a1 `min` a2
                    in (ah-al) :. () == cutBounds ((al,ah) :. ()) ((x+ah) :. ())
            -- @-node:gcross.20091217190104.1527:(al,ah) :. ()
            -- @+node:gcross.20091217190104.1528:(bl,bh) :. (al,ah) :. ()
            ,testProperty "(bl,bh) :. (al,ah) :. ()" $
                \
                (Positive (a1 :: Int))
                (Positive (a2 :: Int))
                (Positive (b1 :: Int))
                (Positive (b2 :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let ah = a1 `max` a2
                        al = a1 `min` a2
                        bh = a1 `max` b2
                        bl = a1 `min` b2
                    in (bh-bl) :. (ah-al) :. () == cutBounds ((bl,bh) :. (al,ah) :. ()) ((y+bh) :. (x+ah) :. ())
            -- @-node:gcross.20091217190104.1528:(bl,bh) :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1529:() :. (al,ah) :. ()
            ,testProperty "() :. (al,ah) :. ()" $
                \
                (Positive (a1 :: Int))
                (Positive (a2 :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let ah = a1 `max` a2
                        al = a1 `min` a2
                    in y :. (ah-al) :. () == cutBounds (() :. (al,ah) :. ()) (y :. (x+ah) :. ())
            -- @-node:gcross.20091217190104.1529:() :. (al,ah) :. ()
            -- @+node:gcross.20091217190104.1530:(al,ah) :. () :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \
                (Positive (a1 :: Int))
                (Positive (a2 :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let ah = a1 `max` a2
                        al = a1 `min` a2
                    in (ah-al) :. x :. () == cutBounds ((al,ah) :. () :. ()) ((y+ah) :. x :. ())
            -- @-node:gcross.20091217190104.1530:(al,ah) :. () :. ()
            -- @+node:gcross.20091217190104.1531:(al,ah) :. b :. ()
            ,testProperty "(al,ah) :. () :. ()" $
                \
                (Positive (a1 :: Int))
                (Positive (a2 :: Int))
                (Positive (b :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let ah = a1 `max` a2
                        al = a1 `min` a2
                    in (ah-al) :. () == cutBounds ((al,ah) :. b :. ()) ((y+ah) :. x :. ())
            -- @-node:gcross.20091217190104.1531:(al,ah) :. b :. ()
            -- @+node:gcross.20091217190104.1532:a :. (bl,bh) :. ()
            ,testProperty "a :. (bl,bh) :. ()" $
                \
                (Positive (b1 :: Int))
                (Positive (b2 :: Int))
                (Positive (a :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let bh = b1 `max` b2
                        bl = b1 `min` b2
                    in (bh-bl) :. () == cutBounds (a :. (bl,bh) :. ()) (y :. (x+bh) :. ())
            -- @-node:gcross.20091217190104.1532:a :. (bl,bh) :. ()
            -- @+node:gcross.20091217190104.1533:(bl,bh,bs) :. ()
            ,testProperty "(bl,bh,bs) :. ()" $
                \
                (Positive (b1 :: Int))
                (Positive (b2 :: Int))
                (Positive (bs :: Int))
                (Positive (a :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let bh = b1 `max` b2
                        bl = b1 `min` b2
                    in ((bh-bl) `div` bs) :. () == cutBounds ((bl,bh,bs) :. ()) ((x+bh) :. ())
            -- @-node:gcross.20091217190104.1533:(bl,bh,bs) :. ()
            -- @+node:gcross.20091217190104.1534:(bl,bh,bs) :. (cl,ch,cs) :. ()
            ,testProperty "(bl,bh,bs) :. (cl,ch,cs) :. ()" $
                \
                (Positive (b1 :: Int))
                (Positive (b2 :: Int))
                (Positive (bs :: Int))
                (Positive (c1 :: Int))
                (Positive (c2 :: Int))
                (Positive (cs :: Int))
                (Positive (a :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                 -> let bh = b1 `max` b2
                        bl = b1 `min` b2
                        ch = c1 `max` c2
                        cl = c1 `min` c2
                    in ((bh-bl) `div` bs) :. ((ch-cl) `div` cs) :. () == cutBounds ((bl,bh,bs) :. (cl,ch,cs) :. ()) ((y+bh) :. (x+ch) :. ())
            -- @-node:gcross.20091217190104.1534:(bl,bh,bs) :. (cl,ch,cs) :. ()
            -- @+node:gcross.20091217190104.1535:a :. (bl,bh) :. (cl,ch,cs) :. ()
            ,testProperty "a :. (bl,bh) :. (cl,ch,cs) :. ()" $
                \
                (Positive (b1 :: Int))
                (Positive (b2 :: Int))
                (Positive (c1 :: Int))
                (Positive (c2 :: Int))
                (Positive (cs :: Int))
                (Positive (a :: Int))
                (Positive (x :: Int))
                (Positive (y :: Int))
                (Positive (z :: Int))
                 -> let bh = b1 `max` b2
                        bl = b1 `min` b2
                        ch = c1 `max` c2
                        cl = c1 `min` c2
                    in (bh-bl) :. ((ch-cl) `div` cs) :. () == cutBounds (a :. (bl,bh) :. (cl,ch,cs) :. ()) (x :. (y+bh) :. (z+ch) :. ())
            -- @-node:gcross.20091217190104.1535:a :. (bl,bh) :. (cl,ch,cs) :. ()
            -- @-others
            ]
        -- @-node:gcross.20091217190104.1519:cutBounds
        -- @-others
        ]
    -- @-node:gcross.20091217190104.1428:Cut
    -- @-others
    -- @-node:gcross.20091217190104.1416:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091217190104.1410:@thin test.hs
-- @-leo
