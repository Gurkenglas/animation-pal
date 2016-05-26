{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Render where

import Graphics.GL.Pal
import Control.Monad.State
import Control.Lens.Extra
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
      eyePos = inv44 viewMat ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO shape) $ do

    forM_ shapeStates $ \r -> do
      uniformV4 uDiffuse (r ^. rndrColor)

      let model = mkTransformation (r ^. rndrPose . posOrientation) (r ^. rndrPose . posPosition)
                  !*! scaleMatrix (r ^. rndrScale)

      drawShape' model projectionView shape

drawShape' :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape' model projectionView shape = do

  let Uniforms{..} = sUniforms shape

  uniformM44 uInverseModel        (inv44 model)
  uniformM44 uModel               model

  uniformM44 uModelViewProjection (projectionView !*! model)

  let vc = geoIndexCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr
