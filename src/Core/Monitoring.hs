
module Core.Monitoring where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Catch
import qualified Control.Monad.Metrics as Metrics
import qualified Data.Text as T

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

