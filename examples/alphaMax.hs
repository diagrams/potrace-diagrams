import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.Potrace
import System.IO.Unsafe
import Codec.Picture

bitmap :: Bitmap
bitmap = unsafePerformIO $ do
  Right png <- readPng "examples/D.png"
  return $ lumaThreshold png 0.5

withAMax :: Double -> Path V2 Double
withAMax a = tracePath' (with & alphaMax .~ a) bitmap

example :: Diagram B
example = frame 10 . fc black . stroke . hsep 10 $ map withAMax [0,0.3,0.5,1,1.3]

make :: Diagram B -> IO ()
make = renderSVG "examples/alphaMax.svg" (mkWidth 1200)

