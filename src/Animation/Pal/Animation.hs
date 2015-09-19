{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Animation.Pal.Animation where

import Control.Lens
import Linear

import Data.Time
import Data.List
import Control.Monad.Trans

-- | The Interpolatable typeclass is just here to allow us to swap out
-- lerp for slerp in the case of Quaternions

class Fractional a => Interpolatable a where
  interp :: a -> a -> Double -> a
  interp fromVal toVal p = fromVal + realToFrac p * (toVal - fromVal)

instance Interpolatable Double
instance Interpolatable Float
instance Fractional a => Interpolatable (V2 a)
instance Fractional a => Interpolatable (V3 a)
instance Fractional a => Interpolatable (V4 a)

instance (RealFloat a) => Interpolatable (Quaternion a) where
  interp fromVal toVal p = slerp fromVal toVal (realToFrac p)

-- | Make your own type Animatable by using chained applications of the anim function
-- to each field, e.g.:
-- > data Object = Object
-- >   { _objPose  :: Pose
-- >   , _objScale :: Float
-- >   , _objColor :: V4 Float
-- >   }
-- > makeLenses ''Object
-- > 
-- > instance Animatable Object where
-- >   animator =  anim (objPose . posPosition)
-- >             . anim (objPose . posOrientation)
-- >             . anim objScale
-- >             . anim objColor
class Animatable a where
  animator :: a -> AnimationFunc a

instance Animatable Double where
  animator _a = anim id
instance Animatable Float where
  animator _a = anim id
instance (Fractional a) => Animatable (V2 a) where
  animator _a = anim id
instance (Fractional a) => Animatable (V3 a) where
  animator _a = anim id
instance (Fractional a) => Animatable (V4 a) where
  animator _a = anim id
instance (RealFloat a) => Animatable (Quaternion a) where
  animator _a = anim id

makeAnimation :: (MonadIO m, Animatable struct) 
              => DiffTime 
              -> struct -> struct -> m (Animation struct)
makeAnimation duration fromA toB = do
  now <- getNow
  return Animation 
      { animFrom = fromA
      , animTo   = toB
      , animFunc = animator fromA
      , animStart = now
      , animDuration = duration
      }

-- | A composable animation func taking a "progress" time and a starting struct,
-- and returning the same progress time along with the modified struct 
-- to be passed to the next animation func.
type AnimationFunc struct = (struct, struct, Double) -> (struct, struct, Double)

data Animation struct = Animation
  { animStart    :: !DiffTime
  , animDuration :: !DiffTime
  , animFunc     :: !(AnimationFunc struct)
  , animFrom     :: !struct
  , animTo       :: !struct
  }

data EvaluatedAnimation struct = EvaluatedAnimation
  { evanResult    :: !struct
  , evanRunning   :: !Bool
  , evanAnimation :: !(Animation struct)
  }

-- | Constructs an animation from a lens. 
-- The animation will take a fromState, a toState and a progress value (0-1)
-- and will interpolate the target of the lens between from/to, 
-- storing the result into the fromState to be passed to the next animation, or returned.
-- 
-- Meant to be composed, like:
-- > let myAnim =  anim (rndrPose . posPosition . _y)
-- >             . anim (rndrPose . posOrientation)
-- >             . anim rndrScale
-- to produce a composite animation of multiple properties simultaneously
-- (you'd define this once for your struct, and then pass it to the Animation type)
anim :: Interpolatable target
     => Lens' struct target 
     -> AnimationFunc struct
anim targetLens (fromStruct, toStruct, progress) = (result, toStruct, progress)
  -- fromStruct is transformed by each composed anim into the final result
  -- progress and toStruct stay constant
  where
    fromVal = fromStruct ^. targetLens
    toVal   = toStruct   ^. targetLens
    value   = interp fromVal toVal (realToFrac progress :: Double)
    result  = fromStruct & targetLens .~ value

-- | Given a "now" time and a list of animations,
-- returns the results of the animations, the still-running animations
-- and any finished animations along with their result
evalAnimations :: DiffTime
               -> [Animation struct]
               -> ([struct], [Animation struct], [EvaluatedAnimation struct])
evalAnimations now animations = (results, map evanAnimation runningAnims, finishedAnims)
  where
    evaledAnims = map (evalAnim now) animations
    -- We get the results from all animations rather than just those still running
    -- so we don't miss a frame when chaining to a new animation
    results = map evanResult evaledAnims
    (runningAnims, finishedAnims) = partition evanRunning evaledAnims

-- | Evaluates an animation at the given "now" time and returns 
-- the result along with whether the animation is over
evalAnim :: DiffTime 
         -> Animation struct 
         -> EvaluatedAnimation struct
evalAnim now animation@Animation{..} = evaluated
  where 
    progress  = realToFrac ((now - animStart) / animDuration)
    eased     = cubicInOut . min 1 $ progress
    evaluated = EvaluatedAnimation
      { evanResult    = view _1 $ animFunc (animFrom, animTo, eased)
      , evanRunning   = progress < 1
      , evanAnimation = animation
      }



continueAnimation :: EvaluatedAnimation struct -> struct -> (Animation struct)
continueAnimation evaledAnim toStruct = 
  Animation
    { animStart    = animStart fromAnim + animDuration fromAnim
    , animDuration = 1
    , animFunc = animFunc fromAnim
    , animFrom = evanResult evaledAnim
    , animTo = toStruct
    }
  where fromAnim = evanAnimation evaledAnim

getNow :: MonadIO m => m DiffTime
getNow = utctDayTime <$> liftIO getCurrentTime

-------------------
-- Easing functions
-------------------

-- | More at https://github.com/warrenm/AHEasing/blob/master/AHEasing/easing.c
quadraticInOut :: (Fractional a, Ord a) => a -> a
quadraticInOut t
  | t < 0.5   = 2 * t ^: 2
  | otherwise = (-2 * t ^: 2) + (4 * t) - 1

cubicInOut :: (Fractional a, Ord a) => a -> a
cubicInOut t
  | t < 0.5   = 4 * t ^: 3
  | otherwise = 0.5 * f ^: 3 + 1
                where f = ((2 * t) - 2)

-- | Hack to default second arg of ^ to Int avoid ambiguous type warnings
(^:) :: Num a => a -> Int -> a
(^:) = (^)
