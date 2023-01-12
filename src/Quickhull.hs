{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

module Quickhull (
  Point,
  Line,
  SegmentedPoints,
  quickhull,
  -- Exported for display
  initialPartition,
  partition,
  -- Exported just for testing
  propagateL,
  shiftHeadFlagsL,
  segmentedScanl1,
  propagateR,
  shiftHeadFlagsR,
  segmentedScanr1,
) where

import Control.Applicative
import qualified Control.Applicative as R
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Debug.Trace
import Data.Array.Accelerate.Smart
import OuterPoints (leftMost, rightMost)
import qualified Prelude as P

-- Points and lines in two-dimensional space
--
type Point = (Int, Int)
type Line = (Point, Point)

-- This algorithm will use a head-flags array to distinguish the different
-- sections of the hull (the two arrays are always the same length).
--
-- A flag value of 'True' indicates that the corresponding point is
-- definitely on the convex hull. The points after the 'True' flag until
-- the next 'True' flag correspond to the points in the same segment, and
-- where the algorithm has not yet decided whether or not those points are
-- on the convex hull.
--
type SegmentedPoints = (Vector Bool, Vector Point)

-- Core implementation
-- -------------------

-- Initialise the algorithm by first partitioning the array into two
-- segments. Locate the left-most (p₁) and right-most (p₂) points. The
-- segment descriptor then consists of the point p₁, followed by all the
-- points above the line (p₁,p₂), followed by the point p₂, and finally all
-- of the points below the line (p₁,p₂).
--
-- To make the rest of the algorithm a bit easier, the point p₁ is again
-- placed at the end of the array.
--
-- We indicate some intermediate values that you might find beneficial to
-- compute.
--
initialPartition :: Acc (Vector Point) -> Acc SegmentedPoints
initialPartition points =
  let
    p1, p2 :: Exp Point
    p1 = error "TODO: locate the left-most point"
    p2 = error "TODO: locate the right-most point"

    isUpper :: Acc (Vector Bool)
    isUpper = error "TODO: determine which points lie above the line (p₁, p₂)"

    isLower :: Acc (Vector Bool)
    isLower = error "TODO: determine which points lie below the line (p₁, p₂)"

    offsetUpper :: Acc (Vector Int)
    countUpper :: Acc (Scalar Int)
    T2 offsetUpper countUpper = error "TODO: number of points above the line and their relative index"

    offsetLower :: Acc (Vector Int)
    countLower :: Acc (Scalar Int)
    T2 offsetLower countLower = error "TODO: number of points below the line and their relative index"

    destination :: Acc (Vector (Maybe DIM1))
    destination = error "TODO: compute the index in the result array for each point (if it is present)"

    newPoints :: Acc (Vector Point)
    newPoints = error "TODO: place each point into its corresponding segment of the result"

    headFlags :: Acc (Vector Bool)
    headFlags = error "TODO: create head flags array demarcating the initial segments"
   in
    T2 headFlags newPoints

-- The core of the algorithm processes all line segments at once in
-- data-parallel. This is similar to the previous partitioning step, except
-- now we are processing many segments at once.
--
-- For each line segment (p₁,p₂) locate the point furthest from that line
-- p₃. This point is on the convex hull. Then determine whether each point
-- p in that segment lies to the left of (p₁,p₃) or the right of (p₂,p₃).
-- These points are undecided.
--
partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 headFlags points) =
  error "TODO: partition"

-- The completed algorithm repeatedly partitions the points until there are
-- no undecided points remaining. What remains is the convex hull.
--
quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull =
  error "TODO: quickhull"

-- Helper functions
-- ----------------

propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedScanl1 const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedScanr1 (\ _ x -> x)

shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL headFlags =
  let
    f :: (Exp a, Exp a, Exp a) -> Exp a
    f (_, _, c) = c
   in
    stencil f wrap headFlags

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR headFlags =
  let
    f :: (Exp a, Exp a, Exp a) -> Exp a
    f (a, _, _) = a
   in
    stencil f wrap headFlags

segmentedScanl1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl1 func headers =
  map snd
    . scanl1 (segmentedL func)
    . zip (map boolToInt headers)
 where
  segmentedL f (T2 _ aV) (T2 bF bV) =
    T2
      bF
      (bF /= 0 ? (bV, f aV bV))

segmentedScanr1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanr1 func headers =
  map snd
    . scanr1 (segmentedR func)
    . zip (map boolToInt headers)
 where
  segmentedR f (T2 bF bV) (T2 _ aV) =
    T2
      bF
      (bF /= 0 ? (bV, f bV aV))

-- Given utility functions
-- -----------------------

pointIsLeftOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsLeftOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y > c
 where
  nx = y1 - y2
  ny = x2 - x1
  c = nx * x1 + ny * y1

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
 where
  nx = y1 - y2
  ny = x2 - x1
  c = nx * x1 + ny * y1

segmented :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
segmented f (T2 aF aV) (T2 bF bV) = T2 (aF || bF) (bF ? (bV, f aV bV))
