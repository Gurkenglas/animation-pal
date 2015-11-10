{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Control.Monad.State.Strict
import Control.Lens.Extra
import Halive.Utils
import Data.Time

import Animation.Pal
import Types
import Render
import Random



spawnCube :: (MonadState World m, MonadIO m) => m ()
spawnCube = do
  
  startTime <- utctDayTime <$> liftIO getCurrentTime

  color <- randomColor

  toShapeState <- randomShapeState

  let shapeAnim = Animation
        { animStart    = startTime
        , animDuration = 1
        , animFunc     = shapeStateAnim
        , animFrom     = newShapeState 
                            & rndrColor .~ color
                            & rndrPose . posOrientation .~ (axisAngle (V3 0 1 0) 0)
        , animTo       = toShapeState
        }
  wldAnimations <>= [shapeAnim]

main :: IO ()
main = do
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "VRPal" NoGCPerFrame []

  -- Set up our cube resources
  cubeProg <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo  <- cubeGeometry (1 :: V3 GLfloat) (V3 1 1 1)
  shape    <- makeShape cubeGeo cubeProg

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  
  useProgram (sProgram shape)

  
  let world = World (newPose {_posPosition = V3 0 0 5}) []

  -- onSpawnTimer <- makeTimer 0.001

  void . flip runStateT world . whileWindow gpWindow $ do
    -- applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer
      onKeyDown e Key'E $ replicateM_ 100 spawnCube
    
    -- onSpawnTimer spawnCube

    now <- utctDayTime <$> liftIO getCurrentTime

    (shapeStates, runningAnims, finishedEvaledAnims) <- evalAnimations now <$> use wldAnimations
    
    -- Whenever an animation finishes, create a new animation to continue it    
    newAnims <- forM finishedEvaledAnims $ \finishedAnim -> 
      -- Animate from the final state of the last animation to the new state
      continueAnimation finishedAnim <$> liftIO randomShapeState

    wldAnimations .= runningAnims ++ newAnims
    
    viewMat <- viewMatrixFromPose <$> use wldPlayer
    renderWith vrPal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shape shapeStates)



