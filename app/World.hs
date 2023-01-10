{-# LANGUAGE BangPatterns #-}

module World where

import Quickhull

import Data.Array.Accelerate                                        ( Vector, Z(..), (:.)(..) )
import Graphics.Gloss.Interface.IO.Interact                         hiding ( Vector, Point )
import qualified Data.Array.Accelerate                              as A


data World = World
  { screenSize    :: !Int
  , pointMinX     :: !Int
  , pointMinY     :: !Int
  , pointMaxX     :: !Int
  , pointMaxY     :: !Int
  , initialPoints :: !(Vector (Int,Int))
  , previousEvent :: !(Maybe Key)
  , state         :: !S
  }

data S
  = S0 !(Vector Point)
  | S1 !SegmentedPoints !Bool !S

initWorld :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> World
initWorld screen minx miny maxx maxy points =
  let v = A.fromList (Z :. length points) points
   in World screen minx miny maxx maxy v Nothing (S0 v)

