{-# LANGUAGE Arrows #-}

-- | Compute the value of pi by generating random number. Using o Storable vector thanks to Dunai MSF that can handle IO.
module Main where

import Data.Bool (bool)
import Data.Vector.Storable.Mutable as St
import Data.Word
import Dunai.Gloss
import System.Random (randomRIO)

s :: Int
s = 200

r :: Double
r = fromIntegral s

unitR = arrM $ const $ randomRIO (0, r)

toRGBA ptr = bitmapOfForeignPtr s s (BitmapFormat TopToBottom PxRGBA) ptr False

net vec = proc () -> do
  i <- count -< ()
  x <- unitR -< ()
  y <- unitR -< ()
  let isinside = x ** 2 + y ** 2 < r ** 2
  sumpi <- sumS -< if isinside then 1 else 0
  arrM (\p -> write vec p 0) -< 4 * (floor y * s + floor x) + if isinside then 0 else 1
  let pi = 4 * sumpi / i :: Double
  returnA -< (pi, i)

network vec = proc _ -> do
  (pi, i) <- repeatMStream 100 $ net vec -< ()
  returnA -< draw ptr pi i
  where
    (ptr, _) = unsafeToForeignPtr0 vec

draw ptr pi i =
  Pictures [toRGBA ptr, Translate (-100) 150 . Scale 0.1 0.1 . Text $ "pi: 4*#red/(#red + #blue) = " <> show pi, Translate (-100) 130 . Scale 0.1 0.1 . Text $ "sqrt(count): " <> show (sqrt i)]

main :: IO ()
main = do
  vec <- St.replicate (s * s * 4) 255
  playDunaiIO (InWindow "Random Pi" (800, 600) (100, 100)) white 60 (network vec)
