{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagarms.Potrace
-- Copyright   :  (c) 2015-2018 Christopher Chalmers
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

import Data.Default
import Data.Sequence
import Geometry as G
import Graphics.Potrace as P

-- | Trace the bitmap image to a path using potrace with 'def'
--   parameters.
tracePath :: OrderedField n => Bitmap -> Path V2 n
tracePath = tracePath' def

-- | Trace the bitmap image to a path curves using potrace with given
--   parameters.
tracePath' :: OrderedField n => Parameters -> Bitmap -> Path V2 n
tracePath' p bm = Path . fromList $ map (mapLoc ClosedTrail . loop) (trace' p bm)

-- Internals -----------------------------------------------------------

-- | Convert a potrace point to a diagrams one.
pt :: Fractional n => P.P2 -> G.P2 n
pt (P.P2 x y) = mkP2 (realToFrac x) (realToFrac y)
{-# INLINE pt #-}

-- | Convert a potrace curve to a diagrams loop.
loop :: OrderedField n => Curve -> Located (Loop V2 n)
loop (P.Curve p0 segs) = closeLine (lineFromSegments (go p0' segs)) `at` p0'
  where
    p0' = pt p0
    go p (s:ss) = case s of
      P.Corner v b   -> straight (pt v .-. p) : straight (pt b .-. pt v) : go (pt b) ss
      P.Bezier u v b -> bezier3 (pt u .-. p) (pt v .-. p) (pt b .-. p) : go (pt b) ss
    go _ []     = []
