{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Render where

import Game.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens

import Data.Maybe

import Types

render :: (MonadIO m, MonadState World m) 
       => Shape Uniforms
       -> [ShapeState]
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shape shapeStates projection viewMat = do
  let Uniforms{..} = sUniforms shape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO shape) $ do

    forM_ shapeStates $ \r -> do
      uniformV4 uDiffuse (r ^. rndrColor)

      let model = mkTransformation (r ^. rndrPose . posOrientation) (r ^. rndrPose . posPosition) 
                  !*! scaleMatrix (r ^. rndrScale)

      drawShape model projectionView shape

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  uniformM44 uModelViewProjection (projectionView !*! model)

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr
