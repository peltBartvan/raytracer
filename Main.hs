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
black = const (0, 0, 0)

whitescreen :: Observer
whitescreen _ = ImageRGB8 (generateImage (\x y -> PixelRGB8 0xff 0xff 0xff) 300 300)


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  savePngImage "whitescreen.png" (whitescreen black)
