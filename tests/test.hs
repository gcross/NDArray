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
    -- @-others
    -- @-node:gcross.20091217190104.1416:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091217190104.1410:@thin test.hs
-- @-leo
