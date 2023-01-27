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
import Data.List (head)
import Debug.Trace
import qualified Prelude as P
import qualified Utils as U
import Data.Array.Accelerate.Data.Maybe (fromJust, isJust)

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

    isUpper, isLower :: Acc (Vector Bool)
    isUpper = map (pointIsLeftOfLine (T2 p1 p2)) points    
    isLower = map (pointIsLeftOfLine (T2 p2 p1)) points

    offsetUpper, offsetLower :: Acc (Vector Int)
    countUpper,countLower :: Acc (Scalar Int)
    T2 offsetUpper countUpper = compact isUpper (U.indices points)
    T2 offsetLower countLower = compact isLower (U.indices points)

    destination :: Acc (Vector (Maybe DIM1))
    destination =
      let
        -- an empty vector of indices
        empty :: Acc (Vector (Maybe DIM1))
        empty = fill (shape points) Nothing_

        newPointIndices :: Exp Int -> Exp Int -> Acc (Vector (Maybe DIM1))
        newPointIndices sh offset = map (Just_ . I1 ) $ enumFromN (I1 sh) offset

        uppers, lowers :: Acc (Vector (Maybe DIM1))
        uppers = newPointIndices (the countUpper) 1
        lowers = newPointIndices (the countLower) (2 + the countUpper)

       in
        scatter
          offsetLower
          (scatter offsetUpper empty uppers)
          lowers

    newPoints :: Acc (Vector Point)
    newPoints =
      let
        offset1 = the countUpper + 1
        offset2 = offset1 + the countLower + 1
        totalLength = offset2 + 1

        p1p2 :: Acc (Vector Point)
        p1p2 = 
          generate
            (I1 (3 + the countLower + the countUpper))
            ( \(I1 ix) ->
                if ix == 0 || ix == (2 + the countLower + the countUpper)
                  then p1
                  else if ix == (the countUpper + 1) 
                    then p2 
                    else undef
            )
       in
        permute const p1p2 (destination !) points

    headFlags :: Acc (Vector Bool)
    headFlags =
      generate
        (I1 (3 + the countLower + the countUpper))
        ( \(I1 ix) ->
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
  let
    p1s, p2s :: Acc (Vector Point)
    p1s = propagateL headFlags points
    p2s = propagateR headFlags points

    distanceToLine :: Acc (Vector Int)
    distanceToLine = zipWith nonNormalizedDistance (zip p1s p2s) points

    indices :: Acc (Vector Int)
    indices = U.indices points
    
    furthest :: Exp (Point, Int) -> Exp (Point, Int) -> Exp (Point, Int)
    furthest pA pB = (snd pB < snd pA) ? (pA, pB)

    p3s :: Acc (Vector Point)
    p3s = map fst $ segFoldRepl furthest headFlags (zip points distanceToLine)

    isLeftOfTri, isRightOfTri :: Acc (Vector Bool)
    isLeftOfTri = zipWith pointIsLeftOfLine (zip p1s p3s) points
    isRightOfTri = zipWith pointIsLeftOfLine (zip p3s p2s) points

    isP3 = zipWith3 (\hf p p3 -> not hf && p == p3) headFlags points p3s

    -- absolute index of the start of the current segment
    segmentStart :: Acc (Vector Int)
    segmentStart = afst $ compact headFlags indices

    segmentIndices :: Acc (Vector Int)
    segmentIndices = map (\x -> x - 1) $ scanl1 (+) (map boolToInt headFlags)
    
    idxsLeftOfTri, idxsRightOfTri, idxsP3 :: Acc (Vector Int)
    idxsLeftOfTri     = afst $ compact isLeftOfTri indices
    idxsRightOfTri    = afst $ compact isRightOfTri indices
    idxsP3            = afst $ compact isP3 indices

    segIdxsLeftOfTri, segIdxsRightOfTri, segIdxsP3 :: Acc (Vector Int)
    segIdxsLeftOfTri  = afst $ compact isLeftOfTri segmentIndices
    segIdxsRightOfTri = afst $ compact isRightOfTri segmentIndices
    segIdxsP3         = afst $ compact isP3 segmentIndices

    empty :: Acc (Vector (Maybe DIM1))
    empty = fill (shape points) Nothing_

    relativeIndicesLeft, relativeIndicesRight, relativeIndicesP :: Acc (Vector Int)
    relativeIndicesLeft   = segmentRelativeIndices (diffDetect segIdxsLeftOfTri)
    relativeIndicesRight  = segmentRelativeIndices (diffDetect segIdxsRightOfTri)
    relativeIndicesP      = segmentRelativeIndices (diffDetect segIdxsP3)
    
    countLeftOfTri :: Acc (Vector Int)
    countLeftOfTri = init $ afst $ segFoldCompact (+) headFlags (map boolToInt isLeftOfTri)
    
    finalIndexLeft  = map (Just_ . I1) 
                    $ zipWith 
                        (\a i -> a + (segmentStart ! I1 i)) 
                        relativeIndicesLeft 
                        segIdxsLeftOfTri

    finalIndexRight = map (Just_ . I1) 
                    $ zipWith 
                        (\a i -> a + (segmentStart ! I1 i) + (countLeftOfTri ! I1 i) + 1) 
                        relativeIndicesRight 
                        segIdxsRightOfTri

    p3FinalIndex    = map (Just_ . I1) 
                    $ zipWith 
                        (\a i -> a + (segmentStart ! I1 i) + (countLeftOfTri ! I1 i)) 
                        relativeIndicesP 
                        segIdxsP3
                        
    p3Scatter = scatter idxsP3 empty p3FinalIndex

    destination :: Acc (Vector (Maybe DIM1))
    destination = let
      fscatter idxs fidxs arr = scatter idxs arr fidxs
      in
        fscatter idxsRightOfTri finalIndexRight $
        fscatter idxsLeftOfTri finalIndexLeft empty

    newPoints :: Acc (Vector (Maybe Point))
    newPoints =
      let
        hull :: Acc (Vector (Maybe Point))
        hull = zipWith (\p f -> if f then Just_ p else Nothing_) points headFlags
        newHull :: Acc (Vector (Maybe Point))
        newHull = permute const hull (p3Scatter !) (map Just_ p3s)
       in
        permute const newHull (destination !) (map Just_ points)
    

    newHeadFlags :: Acc (Vector Bool)
    newHeadFlags = permute const headFlags (p3Scatter !) (fill (shape p3s) True_)

    (compHeadFlags, compPoints) 
      = unzip $ map (U.second fromJust) $ afst $ filter (isJust . snd) (zip newHeadFlags newPoints)
    
   in
    T2 compHeadFlags compPoints

quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull =
  init
    . asnd
    . awhile (map not . and . afst) partition
    . initialPartition

segmentRelativeIndices :: Acc (Vector Bool) -> Acc (Vector Int)
segmentRelativeIndices headFlags = segmentedScanl1 (+) headFlags (fill (shape headFlags) 1)

segFoldRepl :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segFoldRepl fn headFlags xs = propagateR (shiftHeadFlagsL headFlags) (segmentedScanl1 fn headFlags xs)

segFoldCompact :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a, Scalar Int)
segFoldCompact fn headFlags xs = compact (shiftHeadFlagsL headFlags) (segmentedScanl1 fn headFlags xs)

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
    stencil f (function $ const True_) headFlags

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

segmentedScanl :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp a -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl func val headers =
  map snd
    . scanl (segmented func) (T2 True_ val)
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

diffDetect :: Acc (Vector Int) -> Acc (Vector Bool)
diffDetect = stencil st clamp
 where
  st :: Stencil3 Int -> Exp Bool
  st (a, b, _) = a /= b 