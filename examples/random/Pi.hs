{-# LANGUAGE Arrows #-}

-- | Compute the value of pi by generating random number.
module Main where

import Dunai.Gloss
import System.Random (RandomGen (split), mkStdGen)
import System.Random.Stateful (uniformR)

main :: IO ()
main = do
  let gg = split $ mkStdGen 1
  playDunai (InWindow "Random Pi" (800, 600) (100, 100)) white 60 (network gg)
  where
    circAt c = flip (uncurry translate) (Color c $ Circle 1)
    r = 100
    unitR = unfold $ uniformR (- r, r)
    network (g0, g1) = proc _ -> do
      i <- count -< ()
      x <- unitR g0 -< ()
      y <- unitR g1 -< ()
      let isinside = x ** 2 + y ** 2 < r ** 2
      sumpi <- sumS -< if isinside then 1 else 0
      poss <- mappendS -< if isinside then ([(x, y)], []) else ([], [(x, y)])
      let pi = 4 * sumpi / i :: Double
      returnA -< draw poss pi
    draw (inside, outside) pi =
      Pictures $
        (circAt red <$> inside)
          <> (circAt blue <$> outside)
          <> [Translate (-100) 150 . Scale 0.1 0.1 . Text $ "pi: 4*#red/(#red + #blue) = " <> show pi]
