
module Utils where
import Data.Array.Accelerate
import qualified Prelude as P

indices :: (Elt a, Shape sh) => Acc (Array sh a) -> Acc (Array sh Int)
indices xs = enumFromN (shape xs) 0

first :: (Elt a1, Elt a2, Elt b) => (Exp a1 -> Exp a2) -> Exp (a1, b) -> Exp (a2, b)
first fn (T2 x y) = T2 (fn x) y

second :: (Elt a, Elt b1, Elt b2) => (Exp b1 -> Exp b2) -> Exp (a, b1) -> Exp (a, b2)
second fn (T2 x y) = T2 x (fn y)



