
module Test.Gen where

import Data.Array.Accelerate.Sugar.Array                            as A
import Data.Array.Accelerate.Sugar.Elt                              as A
import Data.Array.Accelerate.Sugar.Shape                            as A

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Control.Monad


_MAX_SIZE :: Int
_MAX_SIZE = 1024

dim1 :: Gen DIM1
dim1 = (Z :.) <$> Gen.int (Range.linear 0 _MAX_SIZE)

array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen = fromList sh <$> Gen.list (Range.singleton (size sh)) gen

int :: Gen Int
int = Gen.int (Range.linear (-_MAX_SIZE) _MAX_SIZE)

point :: Gen (Int, Int)
point = (,) <$> int <*> int

except :: Gen e -> (e -> Bool) -> Gen e
except gen f  = do
  v <- gen
  when (f v) Gen.discard
  return v

