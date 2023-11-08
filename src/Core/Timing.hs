
module Core.Timing (
    onStartGame,
    afterMove,
    getTimeLeft,
    isTimingEnabled
  ) where

import Control.Monad.State
import Data.Maybe
import System.Clock

import Core.Types
import Core.Board (opposite)

withTimingConfig :: (TimingConfig -> GameM ()) -> GameM ()
withTimingConfig fn = do
  mbTcfg <- gets gTimingConfig
  case mbTcfg of
    Nothing -> return ()
    Just config -> fn config

withTimingConfig' :: a -> (TimingConfig -> GameM a) -> GameM a
withTimingConfig' x fn = do
  mbTcfg <- gets gTimingConfig
  case mbTcfg of
    Nothing -> return x 
    Just config -> fn config

isTimingEnabled :: GameM Bool
isTimingEnabled = do
  mbTcfg <- gets gTimingConfig
  return $ isJust mbTcfg

modifyTiming :: Side -> (TimingState -> TimingState) -> GameM ()
modifyTiming First fn = modify $ \st -> st {gTiming1 = fn (gTiming1 st)}
modifyTiming Second fn = modify $ \st -> st {gTiming2 = fn (gTiming2 st)}

getTiming :: Side -> GameM TimingState
getTiming First = gets gTiming1
getTiming Second = gets gTiming2

getTimeLeft :: Side -> TimeSpec -> GameM Seconds
getTimeLeft side now = do
  timer <- getTiming side
  currentSide <- gets (gsSide . gState)
  if currentSide == side
    then do
      let delta = diffTimeSpec now (tsTimerStarted timer)
          left = tsTimeLeft timer - (sec delta)
      return left
    else return (tsTimeLeft timer)

onStartGame :: TimeSpec -> GameM ()
onStartGame now = withTimingConfig $ \config -> do
  let time = case tcBaseTime config of
               TotalTime n -> n
               PartsTime {tptInitTime = n} -> n
  modifyTiming First $ \st -> st {tsTimeLeft = time, tsTimerStarted = now}
  modifyTiming Second $ \st -> st {tsTimeLeft = time, tsTimerStarted = now}

updateTimeLeft :: TimingConfig -> TimingState -> TimeSpec -> Seconds
updateTimeLeft config@(TimingConfig {tcBaseTime = (TotalTime {})}) st delta =
    tsTimeLeft st - sec delta + tcTimePerMove config
updateTimeLeft config st delta =
    let base = tcBaseTime config
        addTime = tptAdditionalTime base
        timeLeft = tsTimeLeft st - sec delta
        moveNr =
          case tptNextMoves base of
            Nothing -> tsMovesDone st - tptInitMoves base
            Just n -> (tsMovesDone st - tptInitMoves base) `mod` n
    in  if moveNr == 0
          then if tptKeepPrevTime base
                 then timeLeft + addTime
                 else addTime - sec delta
          else timeLeft + tcTimePerMove config

afterMove :: Side -> TimeSpec -> GameM Bool
afterMove side now = withTimingConfig' True $ \config -> do
  -- end of `side` move
  timer <- getTiming side
  let delta = diffTimeSpec now (tsTimerStarted timer)
      left = tsTimeLeft timer - (sec delta)
  if left < 0
    then return False
    else do
      modifyTiming side $ \st ->
        let timeLeft = updateTimeLeft config st delta
        in  st {tsTimeLeft = timeLeft, tsMovesDone = tsMovesDone st + 1}
      -- start of `opposite side` move
      modifyTiming (opposite side) $ \st -> st {tsTimerStarted = now}
      r <- getTiming side
      return True

