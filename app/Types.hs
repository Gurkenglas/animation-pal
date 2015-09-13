{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types where
import Data.Data
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Game.Pal
import Control.Lens
import Animation.Pal

data World = World
  { _wldPlayer      :: Pose
  , _wldAnimations :: [Animation ShapeState]
  }

data ShapeState = ShapeState
  { _rndrPose     :: Pose
  , _rndrColor    :: V4 GLfloat
  , _rndrScale    :: V3 GLfloat
  }
makeLenses ''World
makeLenses ''ShapeState

newShapeState :: ShapeState
newShapeState = ShapeState 
  { _rndrPose = newPose
  , _rndrColor = 1
  , _rndrScale = 1 
  }

shapeStateAnim :: AnimationFunc ShapeState
shapeStateAnim = anim (rndrPose . posPosition)
               . anim (rndrPose . posOrientation)
               . anim rndrScale
               . anim rndrColor





data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  } deriving (Data)




