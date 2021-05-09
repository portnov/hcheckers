
module Core.Monitoring where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Catch
import qualified Control.Monad.Metrics as Metrics
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as H
import qualified System.Metrics as EKG
import Lens.Micro ((^.))
import Text.Printf

import Core.Types

ifMetricsEnabled :: (Monad m, HasMetricsConfig m) => m () -> m ()
ifMetricsEnabled action = do
  enabled <- isMetricsEnabled
  when enabled action

increment :: (MonadIO m, Metrics.MonadMetrics m, HasMetricsConfig m) => T.Text -> m ()
increment name = ifMetricsEnabled $ Metrics.increment name

timed :: (MonadIO m, Metrics.MonadMetrics m, MonadMask m, HasMetricsConfig m) => T.Text -> m a -> m a
timed name action = do
  enabled <- isMetricsEnabled
  if enabled
    then Metrics.timed name action
    else action

distribution :: (MonadIO m, Metrics.MonadMetrics m, HasMetricsConfig m) => T.Text -> Double -> m ()
distribution name x =
  ifMetricsEnabled $ Metrics.distribution name x

getCurrentMetrics :: (MonadIO m, Metrics.MonadMetrics m) => Maybe T.Text -> m EKG.Sample
getCurrentMetrics mbPrefix = do
    metrics <- Metrics.getMetrics
    let store = metrics ^. Metrics.metricsStore
    sample <- liftIO $ EKG.sampleAll store
    let good = case mbPrefix of
                 Just prefix -> H.filterWithKey (\name _ -> prefix `T.isPrefixOf` name) sample
                 Nothing -> sample
    return good

printCurrentMetrics :: (MonadIO m, Metrics.MonadMetrics m) => Maybe T.Text -> m ()
printCurrentMetrics mbPrefix = do
  liftIO $ putStrLn "Metrics:"
  sample <- getCurrentMetrics mbPrefix
  forM_ (H.toList sample) $ \(key, value) ->
    liftIO $ printf "%s\t%s\n" (T.unpack key) (show value)

