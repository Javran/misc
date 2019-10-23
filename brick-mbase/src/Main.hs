module Main
  ( main
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.RWS.Strict
-- import Control.Monad.Trans.Control
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

data RName = RName deriving (Eq, Ord)

type M = RWST Int (Sum Int) Int IO

{-
  this module is an expermiment on how to use monad-control
  to manipulate the monad stack so that the following looks like
  a simple IO action.

  Let's pretend that we know nothing about stepM's implementation,
  but we know how its monad stack looks like.
 -}
stepM :: Char -> M ()
stepM c = do
  tell 1
  sz <- ask
  case c of
    'j' -> modify (+ sz)
    'k' -> modify (subtract sz)
    _ -> pure ()

{-
  As a first step, this is the straightforward approach:
  Since we know exactly what is needed for recovering a stack of monad transformers,
  we just use a tuple to keep all info necessary.
 -}
type MEncode = (Int, Int, Sum Int) -- r, s, w

stepM' :: Char -> MEncode -> IO ((), MEncode)
stepM' c (r,s,w) = do
  (v, s', w') <- runRWST (stepM c) r s
  pure (v, (r, s', w <> w'))

handleEvent :: MEncode -> BrickEvent RName e -> EventM RName (Next MEncode)
handleEvent s e = case e of
  VtyEvent (EvKey (KChar 'q') []) -> halt s
  VtyEvent (EvKey (KChar c) []) ->
    liftIO (snd <$> stepM' c s) >>= continue
  _ -> continue s

ui :: MEncode -> Widget RName
ui (r,s,Sum w) =
  center $
    intBox "r" r <+> intBox "s" s <+> intBox "w" w
  where
    intBox vName i = border $ str (vName <> ": " <> show i)

main :: IO ()
main = do
  let app =
        App
        { appDraw = \s -> [ui s]
        , appHandleEvent = handleEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap defAttr []
        , appChooseCursor = const $ showCursorNamed RName
        }
  print =<< defaultMain app (10, 0, 0)
