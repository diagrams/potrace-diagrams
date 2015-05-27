{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
import Diagrams.Backend.PGF.CmdLine
import Diagrams.Potrace
import Diagrams.Prelude
import Data.Colour.CIE
import Codec.Picture

main :: IO ()
main = mainWith traced

traced :: IO (Diagram B)
traced = do
  Right (ImageRGBA8 (img@(Image w h _))) <- readPng "examples/juicy.png"
  let img' = generateImage (\i j -> pixelAt img (i*6) (j*6)) (w `div` 6) (h `div` 6)
  return $ coloursToDia
    -- It would be nice a have an algorithm that automatically detects
    -- colours in an image.
    [ sRGB24 255 171 38  -- orange
    , sRGB24 73 168 52   -- green
    , sRGB24 147 88 66   -- brown
    , sRGB24 255 255 255 -- white
    ] img' # lw ultraThick # frame 1

toCIE :: Fractional a => Colour a -> V3 a
toCIE (cieXYZView -> (l,a,b)) = V3 l a b

-- | Simple formula to find the "distance" between two colours. A value
--   of around 0.01 corresponds to just noticeable difference (JND).
cie74 :: Floating a => Colour a -> Colour a -> a
cie74 c1 c2 = distance (toCIE c1) (toCIE c2)

rgb8ToColour :: PixelRGBA8 -> Colour Double
rgb8ToColour (PixelRGBA8 r g b _) = sRGB24 r g b

fromColourImage :: (Colour Double -> Bool) -> Image PixelRGBA8 -> Bitmap
fromColourImage f img@(Image w h _) =
  generate w h $ \i j -> f . rgb8ToColour $ pixelAt img i (h - j - 1)

coloursToDia :: [Colour Double] -> Image PixelRGBA8 -> Diagram B
coloursToDia cs img = foldMap colour cs where
  colour c1
    = fc c1 . stroke . tracePath
    $ fromColourImage (\c2 -> cie74 c1 c2 < 0.02) img

