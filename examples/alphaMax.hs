import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude
import Diagrams.Potrace
import Codec.Picture
import Codec.Picture.Types (dynamicMap)

theD :: IO DynamicImage
theD = do
  Right png <- readPng "examples/D.png"
  return png

alphas :: [Double]
alphas =  [0,0.3,0.5,1,1.3]

withAMax :: Bitmap -> Double -> Diagram B -- Path V2 Double
withAMax bm a =
  tracePath' (with & alphaMax .~ a) bm
    # stroke
    # centerXY
    # label ("$\\alpha_{\\rm{max}} = " ++ show a ++ "$")

original :: DynamicImage -> Diagram B
original (ImageRGB8 i) =
  image (fromJuicy (ImageRGB8 $ multiplyImage 8 i))
    # scale (1/8)
    # centerXY
    # label "original"

ds :: DynamicImage -> Diagram B
ds img =
  original img : map (withAMax bm) alphas
    & hsep 10
    & fc black
    & frame 10
  where bm = lumaThreshold img 0.5

main :: IO ()
main = mainWith (ds <$> theD)

--

fromJuicy :: Num n => DynamicImage -> DImage n Embedded
fromJuicy img = DImage (ImageRaster img) w h mempty
  where
    w = dynamicMap imageWidth img
    h = dynamicMap imageHeight img

multiplyImage :: Pixel a => Int -> Image a -> Image a
multiplyImage n img@(Image w h _) =
  generateImage (\i j -> pixelAt img (i `div` n) (j `div` n)) (w*n) (h*n)

label :: String -> Diagram B -> Diagram B
label x d = d === strutY 3 === text x # fontSizeO 8

