{-# LANGUAGE LambdaCase #-}

module Animation.Pal.Timer where
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent.STM
import Control.Concurrent

makeTimer :: (MonadIO m) => Double -> IO (m () -> m ())
makeTimer secs = do
    tickChan <- newTChanIO
    _ <- forkIO . forever $ do
        atomically (writeTChan tickChan ())
        threadDelay (floor (secs * 1e6))
  
    let onTick action = 
            liftIO (atomically (exhaustTChan tickChan)) >>=
                mapM_ (const action)
    return onTick

exhaustTChan :: TChan a -> STM [a]
exhaustTChan chan = tryReadTChan chan >>= \case
    Just a -> (a:) <$> exhaustTChan chan
    Nothing -> return []
