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

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.Smart
import Debug.Trace
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
    p1 = the (fold1 min points)
    p2 = the (fold1 max points)

    isUpper :: Acc (Vector Bool)
    isUpper = map (pointIsLeftOfLine (T2 p1 p2)) points

    isLower :: Acc (Vector Bool)
    isLower = map (pointIsLeftOfLine (T2 p2 p1)) points

    offsetUpper :: Acc (Vector Int)
    countUpper :: Acc (Scalar Int)
    T2 offsetUpper countUpper = compact isUpper (enumFromN (shape points) 0)

    offsetLower :: Acc (Vector Int)
    countLower :: Acc (Scalar Int)
    T2 offsetLower countLower = compact isLower (enumFromN (shape points) 0)

    destination :: Acc (Vector (Maybe DIM1))
    destination =
      let
        -- an empty vector of indices
        empty :: Acc (Vector (Maybe DIM1))
        empty = fill (I1 (length points)) Nothing_

        -- scatter the points into the empty vector according to `fn`

        scatterPoints :: Acc (Scalar Int) -> Exp Int -> Acc (Vector (Maybe DIM1))
        scatterPoints count offset =
          generate (I1 (the count))
          (\(I1 ix) -> Just_ (I1 (ix + offset)))

        uppers, lowers :: Acc (Vector (Maybe DIM1))
        uppers = scatterPoints countUpper 1
        lowers = scatterPoints countLower (2 + the countUpper)
       in
        scatter
          offsetLower
          (scatter offsetUpper empty uppers)
          lowers

    newPoints :: Acc (Vector Point)
    newPoints =
      let
        p1p2 :: Acc (Vector Point)
        p1p2 = generate (I1 (3 + the countLower + the countUpper)) 
          (\(I1 ix) -> 
            if ix == 0 || 
               ix == (2 + the countLower + the countUpper) 
                then p1
            else if ix == (the countUpper + 1) then p2 else T2 0 0)
       in
        permute const p1p2 (destination !) points

    headFlags :: Acc (Vector Bool)
    headFlags = generate (I1 (3 + the countLower + the countUpper)) 
      (\(I1 ix) -> 
        if ix == 0 
        || ix == (2 + the countLower + the countUpper) 
        || ix == (the countUpper + 1) 
          then True_ 
          else False_
      )
   in
    T2 headFlags newPoints

partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 headFlags points) =
  T2 headFlags points

quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull =
  error "TODO: quickhull"

propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedScanl1 const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedScanr1 const

shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL headFlags =
  let
    f :: (Exp a, Exp a, Exp a) -> Exp a
    f (_, _, c) = c
   in
    stencil f (function $ const True_) headFlags

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR headFlags =
  let
    f :: (Exp a, Exp a, Exp a) -> Exp a
    f (a, _, _) = a
   in
    stencil f (function $ const True_)  headFlags

segmentedScanl1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl1 func headers =
  map snd
    . scanl1 (segmented func)
    . zip headers

segmentedScanr1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanr1 func headers =
  map snd
    . scanr1 (flip (segmented func))
    . zip headers

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
