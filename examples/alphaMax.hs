import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.Potrace
import Codec.Picture

bitmap :: IO Bitmap
bitmap = do
  Right png <- readPng "examples/D.png"
  return $ lumaThreshold png 0.5

withAMax :: Bitmap -> Double -> Path V2 Double
withAMax bm a = tracePath' (with & alphaMax .~ a) bm

alphas :: [Double]
alphas =  [0,0.3,0.5,1,1.3]

ds :: IO (Diagram B)
ds = bitmap <&> \bm ->
  map (withAMax bm) alphas
    # hsep 10
    # stroke
    # fc black
    # frame 10

main :: IO ()
main = mainWith ds

