{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.VR.Pal
import Graphics.GL.Pal
import Control.Monad.State.Strict
import Control.Lens.Extra

import Halive.Utils
import Data.Time

import Animation.Pal

import Types
import Render
import Random

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

    let fromShapeState = newShapeState
                          & rndrColor .~ color
                          & rndrPose . posOrientation .~ (axisAngle (V3 0 1 0) 0)
                          & rndrPose . posPosition . _x .~ x
                          & rndrPose . posPosition . _y .~ y

    toShapeState <- ShapeState
        <$> (randomPose <&> posPosition +~ V3 x y 0)
        <*> randomColorWithHue hue
        <*> pure 0

    let shapeAnim = Animation
            { animStart    = startTime
            , animDuration = 1
            , animFunc = animator (undefined :: ShapeState)
            , animFrom = fromShapeState
            , animTo = toShapeState
            }
    wldAnimations <>= [shapeAnim]

main :: IO ()
main = do
    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "VRPal" []

    -- Set up our cube resources
    cubeProg <- createShaderProgram "test/cube.vert" "test/cube.frag"
    cubeGeo  <- cubeGeometry (1 :: V3 GLfloat) (V3 1 1 1)
    shape    <- makeShape cubeGeo cubeProg

    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    useProgram (sProgram shape)


    let world = World (newPose {_posPosition = V3 0 0 10}) []

    onSpawnTimer <- makeTimer 0.001

    void . flip runStateT world . whileWindow gpWindow $ do
        -- applyMouseLook gpWindow wldPlayer
        applyWASD gpWindow wldPlayer
        player <- use wldPlayer
        (headM44, events) <- tickVR vrPal (transformationFromPose player)
        forM_ events $ \case
            GLFWEvent e -> do
                closeOnEscape gpWindow e
                applyGamepadJoystickMovement e wldPlayer
                onKeyDown e Key'E $ replicateM_ 100 spawnCube
            _ -> return ()

        onSpawnTimer spawnCube

        now <- utctDayTime <$> liftIO getCurrentTime

        (shapeStates, newAnims, _finishedAnims) <- evalAnimations now <$> use wldAnimations
        wldAnimations .= newAnims

        renderWith vrPal headM44 $ \projM44 viewM44 -> do
            glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
            render shape shapeStates projM44 viewM44



