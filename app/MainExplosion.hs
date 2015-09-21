{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Game.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear.Extra
import Control.Monad.State.Strict
import Control.Lens.Extra

import Halive.Utils
import Data.Time

import Animation.Pal

import Types
import Render
import Random

import System.Random

import Control.Concurrent
import Control.Concurrent.STM

data Moment = Moment
  { momPitch :: !Float
  , momHue   :: !Float
  , momScale :: !Float
  }

data Pattern = Pattern
  { ptnPitch :: [Float]
  , ptnHue   :: [Float]
  , ptnScale :: [Float]
  }

spawnCube :: (MonadState World m, MonadIO m) => m ()
spawnCube = do
  
  startTime <- utctDayTime <$> liftIO getCurrentTime

  let hue = sin . realToFrac $ startTime
      x = (* 10) . sin . (* 2) . realToFrac $ startTime
      y = (* 10) . cos . (* 2) . realToFrac $ startTime
  color <- randomColorWithHue hue

  toShapeState <- ShapeState 
      <$> ((& posPosition +~ V3 x y 0) <$> randomPose)
      <*> randomColorWithHue hue 
      <*> pure 0

  let shapeAnim = Animation
        { animStart    = startTime
        , animDuration = 1
        , animFunc = shapeStateAnim
        , animFrom = newShapeState 
                      & rndrColor .~ color
                      & rndrPose . posOrientation .~ (axisAngle (V3 0 1 0) 0)
                      & rndrPose . posPosition . _x .~ x
                      & rndrPose . posPosition . _y .~ y
        , animTo = toShapeState
        }
  wldAnimations <>= [shapeAnim]

main :: IO ()
main = do
  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "GamePal" NoGCPerFrame []

  -- Set up our cube resources
  cubeProg <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo  <- cubeGeometry (1 :: V3 GLfloat) (V3 1 1 1)
  shape    <- makeShape cubeGeo cubeProg

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  
  useProgram (sProgram shape)

  
  let world = World (newPose {_posPosition = V3 0 0 5}) []

  onSpawnTimer <- makeTimer 0.001

  void . flip runStateT world . whileWindow gpWindow $ do
    -- applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer
      onKeyDown Key'E e $ replicateM_ 100 spawnCube
    
    onSpawnTimer spawnCube

    now <- utctDayTime <$> liftIO getCurrentTime

    (shapeStates, newAnims, _finishedAnims) <- evalAnimations now <$> use wldAnimations
    wldAnimations .= newAnims
    
    viewMat <- viewMatrixFromPose <$> use wldPlayer
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shape shapeStates)


