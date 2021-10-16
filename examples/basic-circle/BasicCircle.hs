{-# LANGUAGE Arrows #-}

-- | Simple example where a red circle moves to the right with a constant speed of 10 pixel per seconds.
module Main where

import Dunai.Gloss

main :: IO ()
main = playDunai (InWindow "MSF" (800, 600) (100, 100)) white 60 network
  where
    c = Color red $ Circle 100
    v = 10
    network = proc (dt, _) -> do
      t <- sumS -< dt
      returnA -< translate (t * v) 0 c
