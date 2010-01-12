-- @+leo-ver=4-thin
-- @+node:gcross.20100110123138.1697:@thin Classes.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100110123138.1701:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- @-node:gcross.20100110123138.1701:<< Language extensions >>
-- @nl

module Data.NDArray.Classes where

-- @<< Import needed modules >>
-- @+node:gcross.20100110123138.1700:<< Import needed modules >>
import Foreign.Ptr
import Foreign.Storable

import Data.NDArray.Cuts
import Data.NDArray.Indexable
-- @-node:gcross.20100110123138.1700:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100110123138.1698:Classes
-- @+node:gcross.20100110123138.1716:Accessible
class Accessible constructor where
    withNewNDArray ::
        (Indexable indexType
        ,Storable dataType
        )=>
        indexType ->
        (Ptr dataType -> IO a) ->
        IO (constructor indexType dataType,a)
    withNDArray ::
        constructor indexType dataType ->
        (Ptr dataType -> IO a) ->
        IO a
    withContiguousNDArray ::
        constructor indexType dataType ->
        (Ptr dataType -> IO a) ->
        IO a
-- @-node:gcross.20100110123138.1716:Accessible
-- @+node:gcross.20100110123138.1715:Cutable
class Cutable constructor where
    cut ::
        (Cut cut oldIndexType) =>
        cut ->
        constructor oldIndexType dataType ->
        constructor (CutResult cut oldIndexType) dataType
-- @-node:gcross.20100110123138.1715:Cutable
-- @+node:gcross.20100110123138.1699:Queryable
class Queryable ndarrayType where
    type QueryResultType ndarrayType
    ndarrayBaseOffset :: ndarrayType -> Int
    ndarrayShape :: ndarrayType -> QueryResultType ndarrayType
    ndarrayStrides :: ndarrayType -> QueryResultType ndarrayType
-- @-node:gcross.20100110123138.1699:Queryable
-- @-node:gcross.20100110123138.1698:Classes
-- @-others
-- @-node:gcross.20100110123138.1697:@thin Classes.hs
-- @-leo
