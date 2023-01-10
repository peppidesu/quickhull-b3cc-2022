{-# LANGUAGE ParallelListComp #-}

module Draw where

import Config
import World
import Quickhull

import Graphics.Gloss.Interface.IO.Interact                         hiding ( Vector )
import Data.Array.Accelerate                                        ( Vector, toList, fromList, Z(..), (:.)(..) )
import qualified Data.List                                          as L


screenOfPoint
    :: Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> (Int, Int)
    -> (Float, Float)
screenOfPoint screen minx miny maxx maxy (x, y) =
  let
      u  = min minx miny
      v  = fromIntegral (max (maxx - minx) (maxy - miny)) / 2.0
      w  = fromIntegral screen / 2.0
      --
      x' = fromIntegral (x - u) / v - 1.0
      y' = fromIntegral (y - u) / v - 1.0
  in
  (w * x', w * y')

drawWorld :: World -> IO Picture
drawWorld world =
  case state world of
    S0 _      -> return $ drawRest world (fromList (Z:.0) [])
    S1 sp _ p ->
      let fg = drawHull world sp
          bg = case p of
                 S0{} -> blank
                 _    -> drawRest world (snd sp)
      in
      return $ pictures [ bg, fg ]

drawRest :: World -> Vector (Int,Int) -> Picture
drawRest (World screen minx miny maxx maxy initial _ _) hull =
  let points = toList initial L.\\ toList hull
      c      = circle _RADIUS
  in
  pictures [ translate x y c | p <- points
           , let (x,y) = screenOfPoint screen minx miny maxx maxy p ]

drawHull :: World -> SegmentedPoints -> Picture
drawHull (World screen minx miny maxx maxy _ _ _) sp =
  let
      loc    = screenOfPoint screen minx miny maxx maxy
      (p,ss) = segments sp
      p'     = map loc p
      o      = color orange (circleSolid _RADIUS)
  in
  pictures $ [ pictures [ translate x y z | (x,y) <- map loc seg ]
               | seg <- ss
               | c <- colours, let z = color c (circleSolid _RADIUS) ]
          ++ [ translate x y o | (x,y) <- p' ]
          ++ [ color orange (line p') ]

segments :: SegmentedPoints -> ([(Int,Int)], [[(Int,Int)]])
segments (flags, points) =
  let sps    = zip (toList flags) (toList points)
      (_,p1) = L.span (not . fst) sps

      go []     = ([], [])
      go (x:xs) =
        let (u,v)  = L.span (not . fst) xs
            (p,ps) = go v
        in
        (snd x:p, map snd u : ps)
  in
  go p1

colours :: [Color]
colours =
  let as = [ azure, chartreuse, violet, rose, aquamarine ]
      bs = map light as
      cs = map dark  as
  in
  cycle (as ++ bs ++ cs)

