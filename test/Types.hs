{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types where
import Graphics.GL.Pal
import Control.Lens.Extra
import Animation.Pal

data World = World
  { _wldPlayer     :: !(Pose GLfloat)
  , _wldAnimations :: ![Animation ShapeState]
  }

data ShapeState = ShapeState
  { _rndrPose     :: !(Pose GLfloat)
  , _rndrColor    :: !(V4 GLfloat)
  , _rndrScale    :: !(V3 GLfloat)
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

instance Animatable ShapeState where
    animator _a = shapeStateAnim



data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  } deriving (Data)




