-- @+leo-ver=4-thin
-- @+node:gcross.20091217190104.1391:@thin Setup.hs
-- @@language Haskell

import Blueprint.Tools.GHC.Main

main =
    simpleDefaultMain
        ("","sources")
        (Just (
         ("","tests"),
         [],
         ["HUnit == 1.*"
         ,"QuickCheck == 2.*"
         ,"test-framework == 0.2.*"
         ,"test-framework-hunit == 0.2.*"
         ,"test-framework-quickcheck2 == 0.2.*"
         ,"random == 1.*"
         ]
        ))
        []
-- @-node:gcross.20091217190104.1391:@thin Setup.hs
-- @-leo
