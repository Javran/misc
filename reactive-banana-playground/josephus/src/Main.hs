module Main
  ( main
  )
where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Data.Function
import Data.List

{-
  (WIP) Simulation of Josephus problem (https://en.wikipedia.org/wiki/Josephus_problem)
  with reactive-banana.
 -}

{-
  Design draft:
  - every thread simulates one people
  - every thread keeps the list of people that are still alive
  - we'll need a ticking mechanism, so that every time it ticks,
    exactly one people announces a `Countdown i-1`, if it is previously `Countdown i`
    from previous person.
  - whenever a thread counts to 0, it stops its loop.
  - all counting and ticking are received by all living members.
  - when a thread finds out that it is the only living member, it announces that it is the winner.

  example sequence of events (n = 4, people = 2)
  > Start, Tick, Countdown 4 0, Tick, Countdown 3 1, Tick, Countdown 2 0, Tick, Countdown 1 1, Tick,
  > Countdown 0 0, Execute 0, Tick, Won 1

 -}

data Message
  = Start {num :: Int} -- a special message for starting the game, first person should start counting down from this number.
  | Countdown {num :: Int, who :: Int} -- one person announces a number
  | Tick -- next tick
  | Execute {who :: Int} -- one person gets executed.
  | Won {who :: Int} -- the last living member.
    deriving (Show)

person :: Int -> [Int] -> Chan Message -> (Message -> IO ()) -> IO ()
person myId allPeople recvChan sendMessage = do
  initMsg <- readChan recvChan
  case initMsg of
    Start k -> do
      let amINextInit = myId == 0
      fix
        (\loop amINext curCount livings -> do
           msg <- readChan recvChan
           case msg of
             Start {} -> error "not expecting Start."
             Countdown {num = curNum, who = w} ->
               if curNum /= curCount
                 then error $ "Expect vs actual counting: " <> show (curCount, curNum)
                 else
                   let (_, nextId) : _ = filter ((== w) . fst) $ zip livings (tail $ cycle livings)
                       amINext' = myId == nextId
                       curCount' = if curCount - 1 == 0 then k else curCount - 1
                    in loop amINext' curCount' livings
             Tick -> do
               if length livings == 1
                 then sendMessage (Won myId) >> loop amINext curCount livings
                 else do
                   when amINext $
                     sendMessage (Countdown curCount myId)
                   loop amINext curCount livings
             Execute w ->
               if w == myId
                 then pure ()
                 else
                   let livings' = delete w livings
                       (_, nextId) : _ = filter ((== w) . fst) $ zip livings (tail $ cycle livings)
                       amINext' = myId == nextId
                   in loop amINext' k livings'
             Won w ->
               if w == myId
                 then pure ()
                 else error $ "thread " <> show myId <> " should have been executed.")
        amINextInit
        k
        allPeople
    _ -> error "expecting only Start."


{-
  TODO: a driver that sends Start, Tick and Execute.
 -}
driver recvChan sendMessage k = do
  sendMessage (Start k)
  fix $ \loop -> do
    threadDelay 1000
    msg <- readChan recvChan
    case msg of
      Start _ -> do
        sendMessage Tick
        loop
      Countdown {num = curNum , who = w} -> do
        when (curNum == 1) $
          sendMessage $ Execute w
        sendMessage Tick
        loop
      Tick {} -> loop
      Execute {} -> loop
      Won {} -> pure ()
        -- putStrLn $ "Simulation done, winner is " <> show w

joseph n k = do
  noisyChan <- newChan
  driverChan <- newChan
  peopleChans <- replicateM n newChan
  let allPeople = [0..n-1]
      sendToAll msg = mapM_ (\ch -> writeChan ch msg) (noisyChan : driverChan : peopleChans)
  _hdrs <- mapM (\(pId, ch) -> async $ person pId allPeople ch sendToAll) (zip allPeople peopleChans)
  _ <- async $ forever $ do
    msg <- readChan noisyChan
    print msg
  hDriver <- async $ driver driverChan sendToAll k
  wait hDriver

main :: IO ()
main = joseph 36 12
