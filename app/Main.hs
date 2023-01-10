{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Main where

import Config
import Draw
import Event
import Parser
import World
import Quickhull

import Control.Monad
import Criterion.Main
import Graphics.Gloss.Interface.IO.Interact
import Options.Applicative
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified Data.Vector.Unboxed                                as U

#if defined(ACCELERATE_LLVM_NATIVE_BACKEND)
import Data.Array.Accelerate.LLVM.Native
#elif defined(ACCELERATE_LLVM_PTX_BACKEND)
import Data.Array.Accelerate.LLVM.PTX
#else
import Data.Array.Accelerate.Interpreter
#endif

main :: IO ()
main = do
  opts <- customExecParser (prefs disambiguate) options
  pts  <- case optInput opts of
            FileInput file      -> parseFile file
            RandomInput n mseed -> do
              gen <- case mseed of
                       Nothing   -> createSystemRandom
                       Just seed -> initialize (U.singleton seed)
              let pt = round . (* _MAX_SIZE) <$> standard gen
              replicateM n ((,) <$> pt <*> pt)

  let (xs, ys)    = unzip pts
      screen      = _SCREEN + _PAD
      minx        = minimum xs
      miny        = minimum ys
      maxx        = maximum xs
      maxy        = maximum ys
      world0      = initWorld _SCREEN minx miny maxx maxy pts

  case optMode opts of
    GUI ->
      interactIO
        (InWindow "QuickHull" (screen, screen) (10,10))
        white
        world0
        drawWorld
        handleEvent
        (\_ -> return ())

    Bench ->
      let !go = runN quickhull
      in
      runMode (optBench opts) [
          bench "quickhull" $ whnf go (initialPoints world0)
        ]

