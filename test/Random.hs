module Random where
import System.Random
import Graphics.GL.Pal
import Control.Monad.Trans

import Types

randomShapeState :: MonadIO m => m ShapeState
randomShapeState = ShapeState
      <$> randomPose
      <*> randomColor
      <*> randomScale

randomRIO' :: (Random a, MonadIO m) => (a,a) -> m a
randomRIO' = liftIO . randomRIO

randomPose :: MonadIO m => m (Pose GLfloat)
randomPose = Pose
  <$> randomPosition
  <*> randomOrientation
randomPosition :: MonadIO m => m (V3 GLfloat)
randomPosition = V3
  <$> randomRIO' (-10, 10)
  <*> randomRIO' (-10, 10)
  <*> randomRIO' (-10, 10)

randomScale :: MonadIO m => m (V3 GLfloat)
randomScale = V3
  <$> randomRIO' (0, 10)
  <*> randomRIO' (0, 10)
  <*> randomRIO' (0, 10)

randomAxis :: MonadIO m => m (V3 GLfloat)
randomAxis = V3
  <$> randomRIO' (0, 1)
  <*> randomRIO' (0, 1)
  <*> randomRIO' (0, 1)

randomOrientation :: MonadIO m => m (Quaternion GLfloat)
randomOrientation = axisAngle <$> randomAxis <*> randomRIO' (-10, 10)

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = colorHSL
  <$> randomRIO' (0,1)
  <*> randomRIO' (0,1)
  <*> randomRIO' (0,1)

randomColorWithHue :: MonadIO m => GLfloat -> m (V4 GLfloat)
randomColorWithHue hue = colorHSL
  <$> pure hue
  <*> randomRIO' (0,1)
  <*> randomRIO' (0,1)
