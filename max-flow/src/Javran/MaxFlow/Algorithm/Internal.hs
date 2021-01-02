module Javran.MaxFlow.Algorithm.Internal
  ( M
  , RInfo
  , logM
  , showM
  )
where

{-
  Internally shared infrastructure among max flow algorithms
 -}

import Control.Monad.Except
import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.Writer.CPS
import qualified Data.DList as DL
import Data.Monoid
import qualified Data.Text as T
import Javran.MaxFlow.Types

type RInfo = (NetworkRep, CapacityMap)

{-
  We can certainly extend Sum Int to (DList Text, Sum Int)
  to support logging, which is awkward because every `tell` call will
  then consist of wrapping and unwrapping, with placeholder values (mempty),
  which isn't really ideal.
 -}

type M =
  RWST
    RInfo
    (Sum Int)
    Flow
    ( ExceptT
        String
        (Writer (DL.DList T.Text))
    )

logM :: T.Text -> M ()
logM t =
  lift $
    lift $
      Control.Monad.Trans.Writer.CPS.tell $
        DL.singleton t

showM :: Show a => a -> M ()
showM = logM . T.pack . show
