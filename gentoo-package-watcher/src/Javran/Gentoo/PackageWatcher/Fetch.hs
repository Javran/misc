{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Javran.Gentoo.PackageWatcher.Fetch (
  nfFetch,
) where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception as E
import Control.Exception.Safe (catchAny)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client

{-
  Fetch resource, transform to a result and deepseq, any synchronous exceptions
  in the process are wrapped in SomeException,
  which might still contain some thunk but we don't care.
 -}
nfFetch :: forall d. NFData d => Manager -> String -> (BSL.ByteString -> d) -> IO (Either SomeException d)
nfFetch mgr r f =
  catchAny @IO
    ( do
        req <- parseRequest $ "https://gitweb.gentoo.org/repo/gentoo.git/plain/" <> r
        resp <- httpLbs (setRequestCheckStatus req) mgr
        Right <$> (E.evaluate $!! f (responseBody resp))
    )
    (pure . Left)
