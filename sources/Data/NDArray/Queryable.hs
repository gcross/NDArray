-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1697:@thin Queryable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1701:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
-- @-node:gcross.20100110123138.1701:<< Language extensions >>
-- @nl

module Data.NDArray.Queryable where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1700:<< Import needed modules >>
import Data.NDArray.Indexable
-- @-node:gcross.20100110123138.1700:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1698:Classes
-- @+node:gcross.20100110123138.1699:Queryable
class Queryable ndarrayType indexType where
    ndarrayBaseOffset :: ndarrayType -> Int
    ndarrayShape :: ndarrayType -> indexType
    ndarrayStrides :: ndarrayType -> indexType
-- @-node:gcross.20100110123138.1699:Queryable
-- @-node:gcross.20100110123138.1698:Classes
-- @-others
-- @-node:gcross.20100110123138.1697:@thin Queryable.hs
-- @-leo
