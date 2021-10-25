{-# LANGUAGE Arrows #-}

-- | Compute the value of pi by generating random number.
module Main where

import Data.Bool (bool)
import Data.Set (empty, insert, toList)
import Dunai.Gloss
import System.Random (RandomGen (split), mkStdGen, uniformR)

main :: IO ()
main = do
  let gg = split $ mkStdGen 1
  playDunai (InWindow "Random Pi" (800, 600) (100, 100)) white 60 (network gg)
  where
    circAt (isinside, x, y) = translate (fromIntegral x) (fromIntegral y) $ Color (bool blue red isinside) $ Circle 0
    r = 50 :: Double
    unitR = unfold $ uniformR (- r, r)
    net (g0, g1) = proc _ -> do
      i <- count -< ()
      x <- unitR g0 -< ()
      y <- unitR g1 -< ()
      let isinside = x ** 2 + y ** 2 < r ** 2
      sumpi <- sumS -< if isinside then 1 else 0
      poss <- accumulateWith insert empty -< (isinside, round x, round y)
      let pi = 4 * sumpi / i :: Double
      returnA -< (poss, pi, i)
    network gg = proc _ -> do
      (poss, pi, i) <- repeatMStream 100 $ net gg -< ()
      returnA -< draw (toList poss) pi i
    draw poss pi i =
      Pictures $
        (circAt <$> poss)
          <> [Translate (-100) 150 . Scale 0.1 0.1 . Text $ "pi: 4*#red/(#red + #blue) = " <> show pi, Translate (-100) 130 . Scale 0.1 0.1 . Text $ "sqrt(count): " <> show (sqrt i)]
