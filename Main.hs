module Main where

-- import Control.Monad
-- import Control.Monad.ST
-- import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
-- import qualified Data.Array.Repa     as R

import Codec.Picture
import qualified Codec.Picture.Types as M

-- could be nice
import System.Environment (getArgs)
import System.FilePath (replaceExtension)


data Ray = Ray 
  { x    :: Float
  , y    :: Float
  , dxdz :: Float 
  , dydz :: Float
  }

-- Colors are simple tuples of RGB intensities
-- Intensities are positive and unbounded
-- Converting Colors to proper Pixel values is up to an Observer
type Color = (Float, Float, Float)

type Scene = Ray -> Color
type Observer = Scene -> DynamicImage
type Optic = Scene -> Scene

-- Let's start by staring into space
black :: Scene
black = const (0x00, 0x00, 0x00)

-- But first see if we can even render anything...
-- This creates a white picture, nothing fancy 
whitescreen :: Observer
whitescreen _ = ImageRGB8 (generateImage (\x y -> PixelRGB8 0xff 0xff 0xff) 300 300)

-- A very simple camera
pinhole :: Float -> Float -> Int -> Int -> Observer
pinhole hFOV vFOV hRes vRes scene = 
  let
    dxdzfn = \x -> hFOV * (2*(fromIntegral x)/(fromIntegral hRes) - 1.0)
    dydzfn = \y -> -1 * vFOV * (2*(fromIntegral y)/(fromIntegral vRes) - 1.0)
    toPixel (r, g, b) = PixelRGB8 (round r) (round g) (round b)
    myfun = \xImg yImg -> toPixel $ scene (Ray {x = 0, y = 0, dxdz = dxdzfn xImg, dydz = dydzfn yImg})
  in ImageRGB8 (generateImage myfun hRes vRes)

-- Hello world scene
world :: Scene
world ray 
  | dydz ray < 0   = (105, 168, 50 ) -- the ground is green like grass
  | dydz ray < 0.3 = (200, 100, 1  ) -- with some brown tree trunks
  | dydz ray < 0.5 = (60 , 120, 50 ) -- covered in lovely dark green foliage
  | otherwise      = (90 , 192, 230) -- under a bright blue sky


-- Generates an ideal thin lens, working in the paraxial approximation
thinLens :: Float -> Optic
thinLens focalDistance scene = \ray -> scene $ ray { dxdz = (dxdz ray) - (x ray)/focalDistance
                                                   , dydz = (dydz ray) - (y ray)/focalDistance}

-- paraxial propagator
propagate :: Float -> Optic
propagate distance scene = \ray -> scene $ ray { x = (x ray) + distance*(dxdz ray)
                                               , y = (y ray) + distance*(dydz ray)}

-- Sometimes we want the ray to be processed differently depending on its position
-- This function lets us combine different optics while differentiating the rays by a supplied predicate
joinOptics :: (Ray -> Bool) -> Optic -> Optic -> Optic
joinOptics predicate ifTrue ifFalse scene = \ray -> 
  if predicate ray then (ifTrue scene) ray else (ifFalse scene) ray

-- Color filters are nice
coloredFilter :: Color -> Optic
coloredFilter (r', g', b') scene = \ray -> let (r, g, b) = scene ray in (r' * r, g' * g, b' * b)

-- Just give a ray a given color, no further evaluation needed!
setColor :: Color -> Optic
setColor color scene = \ray -> color

-- This probably isn't right..
-- Should get some sleep and think about this again
-- right now it's translating only the ray, not the scene
-- this might be fine, if ray translation is the opposite of scene translation
-- let's sleep on it, nice to start with a useful translation primitive
translate :: Float -> Float -> Optic
translate dx dy scene = \ray -> scene ray {x = dx + x ray, y = dy + y ray}

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let camera = pinhole 1.1 1.1 200 200
  let redFilter = coloredFilter (1, 0, 0)
  savePngImage "whitescreen.png" (whitescreen black)
  savePngImage "helloworld.png" $ camera world
  savePngImage "helloworld_filtered.png" $ (camera . redFilter) world
  savePngImage "helloworld_lensed.png" $ (camera . (propagate 5) . (thinLens 10)) world
  savePngImage "helloworld_lensed_half.png" $ (camera . (propagate 5)  . (joinOptics (\ray -> 0 < x ray) ((thinLens 15) . (propagate 5) . (thinLens 15) . redFilter) id)) world

