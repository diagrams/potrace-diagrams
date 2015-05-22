{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagarms.Potrace
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Trace bitmap images into paths using the potrace library. This module
-- also provides helpers for turning "JuicyPixel" images to bitmap.
--
module Diagrams.Potrace
  (
    -- * Tracing
    tracePath
  , tracePath'

    -- * Bitmaps
  , Bitmap
  , generate
  , fromImage
  , lumaThreshold

  -- * Parameters
  , Parameters (..)
  , TurnPolicy (..)

  -- ** Lenses
  , turdSize
  , turnPolicy
  , alphaMax
  , optTolerance

  -- * Misc
  , loop
  , pt
  )
 where

import Diagrams.Prelude as D
import Graphics.Potrace as P

-- | Trace the bitmap image to a path using potrace with 'def'
--   parameters.
tracePath :: Bitmap -> Path V2 Double
tracePath = tracePath' def

-- | Trace the bitmap image to a path curves using potrace with given
--   parameters.
tracePath' :: Parameters -> Bitmap -> Path V2 Double
tracePath' p bm = Path $ map (mapLoc Trail . loop) (trace' p bm)

-- Internals -----------------------------------------------------------

pt :: P.P2 -> D.P2 Double
pt (P2 x y) = mkP2 x y
-- pt (P2 x y) = mkP2 (realToFrac x) (realToFrac y)

loop :: Curve -> Located (Trail' Loop V2 Double)
loop (P.Curve p0 segs) = closeLine (lineFromSegments (go p0' segs)) `at` p0'
  where
    p0' = pt p0
    -- for some reason the starting point is actaully the very last point.
    go p (s:ss) = case s of
      P.Corner v b   -> straight (pt v .-. p) : straight (pt b .-. pt v) : go (pt b) ss
      P.Bezier u v b -> bezier3 (pt u .-. p) (pt v .-. p) (pt b .-. p) : go (pt b) ss
    go _ []     = []

-- path :: [[P.Segment]] -> Path V2 Double
-- path = Path . map (mapLoc Trail . loop)

-- dia :: Renderable (Path V2 Double) b => P.Bitmap -> QDiagram b V2 Double Any
-- dia = stroke . path . P.pic

-- loop :: Curve (Trail' Loop V2 Double)
-- loop = foldCurve mkP2 corner cubic (, mempty) <&> \(p, ss) ->
--   where
--     corner u v (p,r)  = (p, straight (pt v .-. p) <| straight (pt b .-. pt v) <| r)
--     cubic u v b (p,r) = (p, bezier3 (pt u .-. p) (pt v .-. p) (pt b .-. p) <| go (pt b) ss)

