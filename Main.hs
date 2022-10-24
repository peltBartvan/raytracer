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

-- Attach an optical element to an Observer
-- A camera with a lens is still a camera
(<|)  :: Observer -> Optic -> Observer
-- Would be nice if everything can just be function composition
-- Wishful thinking?
(<|) = (.)

-- Change a Scene by prepending some optical element
(|>)  :: Optic -> Scene -> Scene
opt |> scene = opt scene

-- A combination of optical elements is itself an optical element, they should be combinable
(<|>) :: Optic -> Optic -> Optic
-- Not sure about this, the types check, does this need to be flip (.)?
(<|>) = (.)

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

redFilter :: Optic
redFilter scene = \ray -> let (r, g, b) = scene ray in (r, 0, 0)


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let camera = pinhole 1.0 1.0 200 200
  savePngImage "whitescreen.png" (whitescreen black)
  savePngImage "helloworld.png" $ camera world
  savePngImage "helloworld_filtered.png" $ (camera . redFilter) world
	
