{-# LANGUAGE Arrows #-}

-- | State Monad example where 3 red circles turn around the cursor the cursor.
module Main where

import Data.Maybe (fromMaybe)
import Dunai.Gloss

main :: IO ()
main = playDunai (InWindow "MSF" (800, 600) (100, 100)) white 60 network
  where
    start = (0, 0)
    r = 30
    network = proc (dt, e) -> do
      t <- sumS -< dt
      p <- accumulateWith (flip fromMaybe) start -< e >>= event
      returnA -< draw p t
    event e = case e of
      EventMotion p -> Just p
      EventKey (Char 'q') Down _ _ -> quit
      _ -> Nothing
    draw (x, y) t = Pictures [Translate (x + r * cos (t + i * 2 * pi / 3)) (y + r * sin (t + i * 2 * pi / 3)) $ Color red $ ThickCircle 0 10 | i <- [0 .. 2]]
