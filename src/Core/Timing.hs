
module Core.Timing (
    onStartGame,
    afterMove,
    getTiming,
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

getPlayerTiming :: Side -> GameM TimingState
getPlayerTiming First = gets gTiming1
getPlayerTiming Second = gets gTiming2

getTiming :: Side -> TimeSpec -> GameM Timing
getTiming side now = do
  timer <- getPlayerTiming side
  currentSide <- gets (gsSide . gState)
  if currentSide == side
    then do
      let delta = diffTimeSpec now (tsTimerStarted timer)
          left = tLeft (tsTiming timer) - delta
          passed = tPassed (tsTiming timer) + delta
      return $ Timing passed left
    else return (tsTiming timer)

onStartGame :: TimeSpec -> GameM ()
onStartGame now = withTimingConfig $ \config -> do
  let time = case tcBaseTime config of
               TotalTime n -> n
               PartsTime {tptInitTime = n} -> n
      reset st = st {tsTiming = Timing 0 (TimeSpec time 0), tsTimerStarted = now}
  modifyTiming First reset
  modifyTiming Second reset

updateTiming :: TimingConfig -> TimingState -> TimeSpec -> Timing
updateTiming config@(TimingConfig {tcBaseTime = (TotalTime {})}) st delta =
    Timing {
        tLeft = tLeft (tsTiming st) - delta + TimeSpec (tcTimePerMove config) 0,
        tPassed = tPassed (tsTiming st) + delta
      }
updateTiming config st delta =
    let base = tcBaseTime config
        addTime = TimeSpec (tptAdditionalTime base) 0
        perMove = TimeSpec (tcTimePerMove config) 0
        timeLeft = tLeft (tsTiming st) - delta
        moveNr =
          case tptNextMoves base of
            Nothing -> tsMovesDone st - tptInitMoves base
            Just n -> (tsMovesDone st - tptInitMoves base) `mod` n
        newTimeLeft =
          if moveNr == 0
            then if tptKeepPrevTime base
                   then timeLeft + addTime
                   else addTime - delta
            else timeLeft + perMove
    in  Timing {
            tLeft = newTimeLeft,
            tPassed = tPassed (tsTiming st) + delta
          }

afterMove :: Side -> TimeSpec -> GameM Bool
afterMove side now = withTimingConfig' True $ \config -> do
  -- end of `side` move
  timer <- getPlayerTiming side
  let delta = diffTimeSpec now (tsTimerStarted timer)
      left = tLeft (tsTiming timer) - delta
  if left < 0
    then return False
    else do
      modifyTiming side $ \st ->
        let newTiming = updateTiming config st delta
        in  st {tsTiming = newTiming, tsMovesDone = tsMovesDone st + 1}
      -- start of `opposite side` move
      modifyTiming (opposite side) $ \st -> st {tsTimerStarted = now}
      return True

