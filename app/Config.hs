{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Word
import Options.Applicative
import Options.Applicative.Help.Pretty

import qualified Criterion.Main.Options                             as Criterion


_RADIUS :: Num a => a
_RADIUS = 5

_SCREEN :: Num a => a
_SCREEN = 800

_PAD :: Num a => a
_PAD = 100

_MAX_SIZE :: Num a => a
_MAX_SIZE = 4096

data Options = Options
  { optInput :: Input
  , optMode  :: Mode
  , optBench :: Criterion.Mode
  }

data Input = FileInput !FilePath | RandomInput !Int !(Maybe Word32)
  deriving Show

data Mode = GUI | Bench
  deriving Show

fileInput :: Parser Input
fileInput = FileInput <$> strOption file
  where
    file =  long "file"
         <> short 'f'
         <> metavar "FILENAME"
         <> help "Input file to read point data"

randomInput :: Parser Input
randomInput = RandomInput <$> option auto size <*> optional (option auto seed)
  where
    size =  long "random"
         <> short 'r'
         <> metavar "INT"
         <> value 128
         <> help "Generate random point data"
    seed =  long "seed"
         <> short 's'
         <> metavar "INT"
         <> help "Random number generator seed"

input :: Parser Input
input = fileInput <|> randomInput

mode :: Parser Mode
mode = flag GUI Bench
     $ long "benchmark"
    <> short 'b'
    <> help "Enable benchmark mode"

options :: ParserInfo Options
options =
  let opts = Options <$> input <*> mode <*> Criterion.parseWith Criterion.defaultConfig
      desc = fullDesc <> headerDoc (Just $ red $ bold "INFOB3CC: QuickHull")
                      <> progDescDoc (Just $ vsep [ mempty
                                                  , "Benchmark and visualisation program for the QuickHull algorithm."
                                                  , "Use ARROW KEYS or SPACE and DELETE to step forwards and backwards in the GUI." ])
   in info (opts <**> helper) desc

